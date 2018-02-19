module Blockchain.BlockTemplateBuilder

open Infrastructure
open Consensus.TransactionValidation
open Blockchain.State
open Consensus

let getUTXO = UtxoSetRepository.get

let selectOrderedTransactions chain (session:DatabaseContext.Session) blockNumber acs transactions =
    let contractPath = session.context.contractPath

    let tryAddTransaction (state, added, notAdded, altered) (txHash,tx) =
        validateInContext chain (getUTXO session) contractPath (blockNumber + 1ul)
            state.activeContractSet state.utxoSet txHash tx
        |> function
        | Error (Orphan | ContractNotActive) ->
            (state, added, (txHash,tx)::notAdded, altered)
        | Error _ ->
            (state, added, notAdded, altered)
        | Ok (tx, acs) ->
            let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
            let mempool = MemPool.add txHash tx state.mempool
            ({state with activeContractSet=acs;mempool=mempool;utxoSet=utxoSet}, tx::added, notAdded, true)

    let foldOverTransactions foldState txs =
        List.fold tryAddTransaction foldState txs
        |> fun (s, added, notAdded, altered) -> (s, added, List.rev notAdded, altered)

    let foldUntilUnchanged state txs =
        let rec inner foldState txs =
            match foldOverTransactions foldState txs with
            | (s, added, notAdded, true) -> inner (s, added, [], false) notAdded
            | (s, added, _, false) -> (s, List.rev added)
        inner (state, [], [], false) txs

    let initialState = {
        utxoSet = UtxoSet.asDatabase;
        activeContractSet = acs;
        orphanPool = OrphanPool.create();
        mempool = MemPool.empty
        }

    foldUntilUnchanged initialState <| List.map (fun tx -> (Transaction.hash tx, tx)) transactions


let makeTransactionList chain (session:DatabaseContext.Session) (state:State) =
    let txs = MemPool.getTransactions state.memoryState.mempool
    selectOrderedTransactions chain session state.tipState.tip.header.blockNumber state.tipState.activeContractSet txs