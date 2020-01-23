module Blockchain.BlockTemplateBuilder

open Consensus

open Infrastructure
open ValidationError
open Result
open Consensus.TransactionValidation
open Blockchain.State
open Consensus
open Consensus.Chain
open Consensus.Types
open Functional

module ZData = Zen.Types.Data

let getUTXO = UtxoSetRepository.get
let getContractState = ContractStateRepository.get

let result = Result.ResultBuilder<string>()

let private hasValidData
    ( cgp  : CGP.T                       )
    ( msgBody : Option<Zen.Types.Data.data> )
    =
    let outputs = CGP.Contract.extractPayoutOutputs msgBody |> Option.defaultValue []
    let winner =
        match cgp.payout with
        | Some payout ->
            CGP.Connection.internalizeRecipient payout
        | None ->
            []
    List.sort outputs = List.sort winner 

let private filterValidCgpTxs
    (chain : ChainParameters                    )
    (cgp   : CGP.T                              )
    (txs   : List<TransactionExtended * bigint> )
    : List<TransactionExtended * bigint> =
    
    let exposeCGPWitnesses chain ex =
        ex, CGP.Connection.extractPayoutWitnesses chain ex
    
    let checkWitnessesValidity (ex, cws) =
        let checkDataValidity cw = hasValidData cgp cw.messageBody
        ex, List.filter checkDataValidity cws
    
    let passSingleton (((ex,cws), w) : (TransactionExtended * ContractWitness list) * bigint) =
        match cws with
        | [_] -> Some (ex, w)
        | _ -> None
    
    txs
    |> List.choose (firstMap (exposeCGPWitnesses chain >> checkWitnessesValidity) >> passSingleton)


let private findFirstValidCGPTx
    (chain : ChainParameters                    )
    (cgp   : CGP.T                              )
    (txs   : List<TransactionExtended * bigint> )
    =
    txs
    |> filterValidCgpTxs chain cgp
    |> List.tryHead
                
let removeCGPTxs
    (chain : ChainParameters                    )
    (txs   : List<TransactionExtended * bigint> )
    : List<TransactionExtended * bigint> =
        let filterNonCGP ((ex, _) as p) =
            if ex |> CGP.Connection.extractPayoutWitnesses chain |> List.isEmpty then
                Some p
            else
                None
        txs
        |> List.choose filterNonCGP
        
                
open Logary.Message
              
//TODO: Move Error to typeError instead of string
let private filterCGPExecutions
    (chain : ChainParameters                    )
    (cgp   : CGP.T                              )
    (txs   : List<TransactionExtended * bigint> )
    : List<TransactionExtended * bigint>  =
    
    let payoutWitnesses : List<ContractWitness> =
        txs
        |> List.concatMap (fst >> CGP.Connection.extractPayoutWitnesses chain)
    
    // Either return the 1st valid CGP tx, or remove all the cgp txs
    // (sorry for the convoluted implementation, it's more efficient this way)
    let uniqueCgp =
        match findFirstValidCGPTx chain cgp txs with
        | None ->
            eventX "No valid cgp transaction found, execute the contract correctly"
            |> Log.error
            Error (removeCGPTxs chain txs)
        | Some tx ->
            Ok tx
    
    match cgp.payout with
    | None ->
        // we can safely remove the cgp txs as if there are none it will return an empty list
        removeCGPTxs chain txs
    | Some _ ->
        match payoutWitnesses with
        | [] ->
            eventX "You should create your own contract execution"
            |> Log.error
            txs
        | [_] ->
            uniqueCgp |> Result.fromOk (fun _ -> txs)
        | _ :: _ :: _ ->
            uniqueCgp |> Result.fromOk (fun tx -> tx :: removeCGPTxs chain txs)

// We pre-compute the weights of all transactions in the mempool.
// It may be better to compute the weights when transactions are inserted, but
// doing it when creating a template works reasonably well.
// There's no need to update the UTXO set when adding the weights -- the only
// use of the UTXOs is to dereference all the outputs.
let weights session state =
    MemPool.toList state.memoryState.mempool
    |> Result.traverseResultM (fun ex -> 
        UtxoSet.tryGetOutputs (getUTXO session) state.memoryState.utxoSet ex.tx.inputs
        |> Result.ofOption "could not get outputs"
        <@> TxSkeleton.fromTransaction ex.tx
        >>= Weight.transactionWeight ex.tx
        <@> fun weight -> ex, weight)

let selectOrderedTransactions (chain:Chain.ChainParameters) (session:DatabaseContext.Session) blockNumber timestamp acs contractCache transactions =
    let contractPath = session.context.contractPath
    let maxWeight = chain.maxBlockWeight
    let getUTXO = getUTXO session
    let blockNumber = blockNumber + 1ul

    let tryAddTransaction (state, added, notAdded, altered, weight) (ex,wt) =
        let newWeight = weight+wt
        if newWeight > maxWeight then (state, added, notAdded, false, weight) else
        validateInContext chain getUTXO contractPath blockNumber timestamp
            state.activeContractSet state.contractCache state.utxoSet (getContractState session) ContractStates.asDatabase ex
        |> function
            | Error (Orphan | ContractNotActive) ->
                (state, added, (ex,wt)::notAdded, altered, weight)
            | Error _ ->
                (state, added, notAdded, altered, weight)
            | Ok (tx, acs, contractCache, contractStates) ->
                let utxoSet = UtxoSet.handleTransaction getUTXO ex.txHash ex.tx state.utxoSet
                let mempool = MemPool.add ex state.mempool
                ({state with activeContractSet=acs;mempool=mempool;utxoSet=utxoSet;contractCache=contractCache;contractStates=contractStates}, tx::added, notAdded, true, newWeight)

    let foldOverTransactions foldState txs =
        List.fold tryAddTransaction foldState txs
        |> fun (s, added, notAdded, altered, wt) -> (s, added, List.rev notAdded, altered, wt)

    let foldUntilUnchanged state txs =
        let rec inner foldState txs =
            match foldOverTransactions foldState txs with
            | (s, added, notAdded, true, wt) -> inner (s, added, [], false, wt) notAdded
            | (s, added, _, false, _) -> (s, List.rev added)
        inner (state, [], [], false, 0I) txs

    let initialState = {
        utxoSet = UtxoSet.asDatabase
        activeContractSet = acs
        orphanPool = OrphanPool.create()
        mempool = MemPool.empty
        contractCache = contractCache
        contractStates = ContractStates.asDatabase
        invalidTxHashes = Set.empty
    }

    foldUntilUnchanged initialState transactions


let makeTransactionList chain (session:DatabaseContext.Session) (state:State) timestamp = result {
    let! txs = weights session state
    let txs =
        if CGP.isPayoutBlock chain (state.tipState.tip.header.blockNumber + 1ul) then
            filterCGPExecutions chain state.cgp txs
        else
            txs
        |> List.sortBy (fun (_,wt) -> wt)
    return selectOrderedTransactions chain session state.tipState.tip.header.blockNumber timestamp state.tipState.activeContractSet state.memoryState.contractCache txs
}