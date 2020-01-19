module Blockchain.BlockTemplateBuilder

open Consensus
open ValidationError
open Chain
open Types
open TransactionValidation

open Infrastructure
open Result

open Blockchain.State

open Logary.Message

module ZData = Zen.Types.Data

let result = Result.ResultBuilder<string>()


module private CGP =
    
    open FSharpx.Reader
    
    let reader = FSharpx.Reader.reader
    
    let (<@|) = map
    
    let (|@>) x f = map f x
    
    type Env =
        {
            chain : ChainParameters
            cgp   : CGP.T
        }
        static member getCGP =
            ask |@> fun env -> env.cgp 
        static member getChain =
            ask |@> fun env -> env.chain
        static member getEnv =
            ask
    
    type E<'a> = Reader<Env, 'a>
    
    let hasValidData
        ( msgBody : Option<Zen.Types.Data.data> )
        : E< bool > = reader {
            let! cgp = Env.getCGP
            
            let outputs =
                msgBody
                |> CGP.Contract.extractPayoutOutputs
                |> Option.defaultValue []
            
            let winner =
                cgp.payout
                |> Option.map CGP.Connection.internalizeRecipient
                |> Option.defaultValue []
            
            return List.sort outputs = List.sort winner
        }
    
    let filterValidCgpTxs
        ( xs : List<TransactionExtended * 'a> )
        : E< List<TransactionExtended * 'a> > = reader {
            let! env   = Env.getEnv
            let! chain = Env.getChain
            
            let extractValidCGPWitnesses ex =
                ex
                |> CGP.Connection.extractPayoutWitnesses chain
                |> List.filter (fun cw -> hasValidData cw.messageBody env)
            
            let isValidCgp =
                fst
                >> extractValidCGPWitnesses
                >> List.isSingleton
            
            return List.choose (Option.validateWith isValidCgp) xs
        }
    
    let removeAllCgpTxs
        ( txs : List<TransactionExtended * 'a> )
        : E< List<TransactionExtended * 'a> > = reader {
            let! chain = Env.getChain
            
            let isNonCGP =
                 fst
                 >> CGP.Connection.extractPayoutWitnesses chain
                 >> List.isEmpty
            
            return List.choose (Option.validateWith isNonCGP) txs
        }
    
    let filterCGPExecutions
        ( txs : List<TransactionExtended * 'a> )
        : E< List<TransactionExtended * 'a> > = reader {
            let! chain = Env.getChain
            let! cgp   = Env.getCGP
            
            let payoutWitnesses =
                txs
                |> List.concatMap (fst >> CGP.Connection.extractPayoutWitnesses chain)
            
            // Either return the 1st valid CGP tx, or remove all the cgp txs
            // (sorry for the convoluted implementation, it's more efficient this way)
            let uniqueCgp = reader {
                match! List.tryHead <@| filterValidCgpTxs txs with
                | None ->
                    eventX "No valid cgp transaction found, execute the contract correctly"
                    |> Log.error
                    return! Error <@| removeAllCgpTxs txs
                | Some tx ->
                    return Ok tx
            }
            
            match cgp.payout with
            | None ->
                // we can safely remove the cgp txs as if there are none it will return an empty list
                return! removeAllCgpTxs txs
            | Some _ ->
                match payoutWitnesses with
                | [] ->
                    eventX "You should create your own contract execution"
                    |> Log.error
                    return txs
                | [_] ->
                    return! uniqueCgp |@> Result.fromOk (fun _ -> txs)
                | _ :: _ :: _ ->
                    let! noCgpTxs = removeAllCgpTxs txs
                    return! uniqueCgp |@> Result.fromOk (fun tx -> tx :: noCgpTxs)
        }

// We pre-compute the weights of all transactions in the mempool.
// It may be better to compute the weights when transactions are inserted, but
// doing it when creating a template works reasonably well.
// There's no need to update the UTXO set when adding the weights -- the only
// use of the UTXOs is to dereference all the outputs.
let weights session state =
    MemPool.toList state.memoryState.mempool
    |> Result.traverseResultM (fun ex -> 
        UtxoSet.tryGetOutputs (UtxoSetRepository.get session) state.memoryState.utxoSet ex.tx.inputs
        |> Result.ofOption "could not get outputs"
        <@> TxSkeleton.fromTransaction ex.tx
        >>= Weight.transactionWeight ex.tx
        <@> fun weight -> ex, weight)

let selectOrderedTransactions (chain:Chain.ChainParameters) (session:DatabaseContext.Session) blockNumber timestamp acs contractCache transactions =
    let contractPath = session.context.contractPath
    let maxWeight = chain.maxBlockWeight
    let getUTXO = UtxoSetRepository.get session
    let blockNumber = blockNumber + 1ul

    let tryAddTransaction (state, added, notAdded, altered, weight) (ex,wt) =
        let newWeight = weight+wt
        if newWeight > maxWeight then (state, added, notAdded, false, weight) else
        validateInContext chain getUTXO contractPath blockNumber timestamp
            state.activeContractSet state.contractCache state.utxoSet (ContractStateRepository.get session) ContractStates.asDatabase ex
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
            CGP.filterCGPExecutions txs { chain=chain ; cgp=state.cgp }
        else
            txs
        |> List.sortBy (fun (_,wt) -> wt)
    return selectOrderedTransactions chain session state.tipState.tip.header.blockNumber timestamp state.tipState.activeContractSet state.memoryState.contractCache txs
}