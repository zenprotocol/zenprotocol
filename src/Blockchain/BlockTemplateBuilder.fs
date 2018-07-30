module Blockchain.BlockTemplateBuilder

open Consensus

open Infrastructure
open ValidationError
open Result
open Consensus.TransactionValidation
open Blockchain.State
open Consensus

let getUTXO = UtxoSetRepository.get
let getContractState = ContractStateRepository.get

let result = new Result.ResultBuilder<string>()

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

    let tryAddTransaction (state, added, notAdded, altered, weight) (ex,wt) =
        let newWeight = weight+wt
        if newWeight > maxWeight then (state, added, notAdded, false, weight) else
        validateInContext chain (getUTXO session) contractPath (blockNumber + 1ul) timestamp
            state.activeContractSet state.contractCache state.utxoSet (getContractState session) ContractStates.asDatabase ex
        |> function
            | Error (Orphan | ContractNotActive) ->
                (state, added, (ex,wt)::notAdded, altered, weight)
            | Error _ ->
                (state, added, notAdded, altered, weight)
            | Ok (tx, acs, contractCache, contractStates) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) ex.txHash ex.tx state.utxoSet
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
    }

    foldUntilUnchanged initialState transactions


let makeTransactionList chain (session:DatabaseContext.Session) (state:State) timestamp = result {
    let! txs = weights session state 
    let txs = List.sortBy (fun (_,wt) -> wt) txs
    return selectOrderedTransactions chain session state.tipState.tip.header.blockNumber timestamp state.tipState.activeContractSet state.memoryState.contractCache txs
}