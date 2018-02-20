module Blockchain.TransactionHandler

open Blockchain
open Blockchain
open Blockchain
open Blockchain.EffectsWriter
open Messaging.Events
open Infrastructure
open Consensus
open Consensus.Transaction
open Consensus.TransactionValidation
open State
open Infrastructure.ServiceBus.Agent

let getUTXO = UtxoSetRepository.get

let private validateOrphanTransaction chain session contractPath blockNumber state txHash tx  =
    effectsWriter
        {
            match TransactionValidation.validateInContext chain (getUTXO session) contractPath (blockNumber + 1ul)
                    state.activeContractSet state.utxoSet txHash tx with
            | Ok (tx, acs) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool

                do! publish (TransactionAddedToMemPool (txHash, tx))
                Log.info "Orphan transaction %s added to mempool" (Hash.toString txHash)


                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {state with
                            activeContractSet=acs;
                            mempool=mempool;
                            utxoSet=utxoSet;
                            orphanPool=orphanPool}
            | Error Orphan
            | Error ContractNotActive ->
                // transaction is still orphan, nothing to do
                return state
            | Error BadContract ->
                 Log.info "Previously orphaned transaction %s failed to activate its contract" (Hash.toString txHash)

                 let orphanPool = OrphanPool.remove txHash state.orphanPool
                 return {state with orphanPool = orphanPool}
            | Error error ->
                Log.info "Orphan transaction %s failed validation: %A" (Hash.toString txHash) error

                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {state with orphanPool = orphanPool}
        }

let rec validateOrphanTransactions chain session contractPath blockNumber state =
    effectsWriter {
        let! state' = OrphanPool.foldWriter (validateOrphanTransaction chain session contractPath blockNumber) state state.orphanPool

        // if orphan pool changed we run again until there is no change
        if state'.orphanPool <> state.orphanPool then
            return! validateOrphanTransactions chain session contractPath blockNumber state'
        else
            return state'
    }

let validateInputs chain session contractPath blockNumber txHash tx (state:MemoryState) shouldPublishEvents =
    effectsWriter
        {
            match TransactionValidation.validateInContext chain (getUTXO session) contractPath (blockNumber + 1ul)
                    state.activeContractSet state.utxoSet txHash tx with
            | Error Orphan ->
                let orphanPool = OrphanPool.add txHash tx state.orphanPool

                Log.info "Transaction %s is an orphan. Adding to orphan pool." (Hash.toString txHash)

                return {state with orphanPool = orphanPool}
            | Error ContractNotActive ->
                let orphanPool = OrphanPool.add txHash tx state.orphanPool

                Log.info "Transaction %s tried to run an inactive contract. Adding to orphan pool." (Hash.toString txHash)

                return {state with orphanPool = orphanPool}
            | Error BadContract ->
                 Log.info "Transaction %s failed to activate its contract" (Hash.toString txHash)

                 return state
            | Error error ->
                 Log.info "Transaction %s failed inputs validation: %A" (Hash.toString txHash) error

                 return state
            | Ok (tx, acs) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool

                if shouldPublishEvents then
                    do! publish (TransactionAddedToMemPool (txHash,tx))

                Log.info "Transaction %s added to mempool" (Hash.toString txHash)

                let state = {state with
                                activeContractSet=acs;
                                mempool=mempool;
                                utxoSet=utxoSet;
                             }

                return! validateOrphanTransactions chain session contractPath blockNumber state
        }


let validateTransaction chain session contractPath blockNumber tx (state:MemoryState) =
    effectsWriter {
        let txHash = Transaction.hash tx

        if MemPool.containsTransaction txHash state.mempool ||
           OrphanPool.containsTransaction txHash state.orphanPool ||
           TransactionRepository.isPartOfMainChain session txHash then
            return state
        else
            match TransactionValidation.validateBasic tx with
            | Error error ->
                Log.info "Transaction %s failed basic validation: %A" (Hash.toString txHash) error

                return state
            | Ok tx ->
                return! validateInputs chain session contractPath blockNumber txHash tx state true
    }

let executeContract session txSkeleton cHash command data returnAddress state =
    let isInTxSkeleton (txSkeleton:TxSkeleton.T) (outpoint,_)  =
        List.exists (fun input ->
            match input with
            | TxSkeleton.Mint _ -> false
            | TxSkeleton.PointedOutput (outpoint',_) -> outpoint' = outpoint) txSkeleton.pInputs

    let rec run cHash command data (txSkeleton:TxSkeleton.T) witnesses totalCost =
        match ActiveContractSet.tryFind cHash state.activeContractSet with
        | None -> Error "Contract not active"
        | Some contract ->
            let contractWallet =
                ContractUtxoRepository.getContractUtxo session cHash state.utxoSet
                |> List.reject (isInTxSkeleton txSkeleton)

            let cost = Contract.getCost contract command data (Some returnAddress) contractWallet txSkeleton
            let totalCost = cost + totalCost

            Contract.run contract command data (Some returnAddress) contractWallet txSkeleton
            |> Result.bind (fun (tx, message) ->
                TxSkeleton.checkPrefix txSkeleton tx
                |> Result.bind (fun finalTxSkeleton ->
                    let witnesses = List.add (TxSkeleton.getContractWitness contract.hash command data returnAddress txSkeleton finalTxSkeleton) witnesses

                    match message with
                    | Some {cHash=cHash; command=command; data=data} ->
                        run cHash command data finalTxSkeleton witnesses totalCost
                    | None ->
                        Ok (finalTxSkeleton, witnesses, totalCost)
                )
            )

    run cHash command data txSkeleton [] 0I
    |> Result.map (fun (finalTxSkeleton, witnesses, totalCost) ->
        Log.info "Running contract chain with cost: %A" totalCost

        let tx =
            Transaction.fromTxSkeleton finalTxSkeleton
            |> Transaction.addWitnesses witnesses

        tx
    )

