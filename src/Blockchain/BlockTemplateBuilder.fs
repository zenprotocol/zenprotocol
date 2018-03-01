module Blockchain.BlockTemplateBuilder

open Infrastructure
open Consensus.TransactionValidation
open Blockchain.State
open Consensus

let getUTXO = UtxoSetRepository.get

// We pre-compute the weights of all transactions in the mempool.
// It may be better to compute the weights when transactions are inserted, but
// doing it when creating a template works reasonably well.
// There's no need to update the UTXO set when adding the weights -- the only
// use of the UTXOs is to dereference all the outputs.
let weights session state =
    let resMap =
        Map.map
            (fun _ tx ->
                (tx, Weight.transactionWeight (getUTXO session) state.memoryState.utxoSet tx))
            state.memoryState.mempool    
    resMap
    |> Map.toList
    |> fun sq ->
            [
            for (txHash, (tx, wtRes)) in sq do
                match wtRes with
                | Ok wt -> yield (txHash, tx, wt)
                | _ -> ()
            ]

let selectOrderedTransactions chain (session:DatabaseContext.Session) blockNumber acs transactions =
    let contractPath = session.context.contractPath
    let maxWeight = Chain.getMaximumBlockWeight chain

    let tryAddTransaction (state, added, notAdded, altered, weight) (txHash,tx,wt) =
        let newWeight = weight+wt
        if newWeight > maxWeight then (state, added, notAdded, false, weight) else
        validateInContext chain (getUTXO session) contractPath (blockNumber + 1ul)
            state.activeContractSet state.utxoSet txHash tx
        |> function
            | Error (Orphan | ContractNotActive) ->
                (state, added, (txHash,tx,wt)::notAdded, altered, weight)
            | Error _ ->
                (state, added, notAdded, altered, weight)
            | Ok (tx, acs) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool
                ({state with activeContractSet=acs;mempool=mempool;utxoSet=utxoSet}, tx::added, notAdded, true, newWeight)

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
        utxoSet = UtxoSet.asDatabase;
        activeContractSet = acs;
        orphanPool = OrphanPool.create();
        mempool = MemPool.empty
        }

    foldUntilUnchanged initialState transactions


let makeTransactionList chain (session:DatabaseContext.Session) (state:State) =
    let txs = weights session state |> List.sortBy (fun (_,_,wt) -> -wt)
    selectOrderedTransactions chain session state.tipState.tip.header.blockNumber state.tipState.activeContractSet txs