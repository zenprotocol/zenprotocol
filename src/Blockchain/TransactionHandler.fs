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

let private validateOrphanTransaction session contractPath blockNumber state txHash tx  =
    effectsWriter
        {
            match TransactionValidation.validateInContext (getUTXO session) contractPath (blockNumber + 1ul)
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

let rec validateOrphanTransactions session contractPath blockNumber state =
    effectsWriter {
        let! state' = OrphanPool.foldWriter (validateOrphanTransaction session contractPath blockNumber) state state.orphanPool

        // if orphan pool changed we run again until there is no change
        if state'.orphanPool <> state.orphanPool then
            return! validateOrphanTransactions session contractPath blockNumber state'
        else
            return state'
    }

let validateInputs session contractPath blockNumber txHash tx (state:MemoryState) shouldPublishEvents =
    effectsWriter
        {
            match TransactionValidation.validateInContext (getUTXO session) contractPath (blockNumber + 1ul)
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

                return! validateOrphanTransactions session contractPath blockNumber state
        }


let validateTransaction session contractPath blockNumber tx (state:MemoryState) =
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
                return! validateInputs session contractPath blockNumber txHash tx state true
    }

let executeContract session txSkeleton cHash command data returnAddress state =
    let checkACS cHash =
        match ActiveContractSet.tryFind cHash state.activeContractSet with
        | Some contract -> Ok contract
        | None -> Error "Contract not active"

    let contractWallet = ContractUtxoRepository.getContractUtxo session cHash state.utxoSet

    checkACS cHash
    |> Result.bind (fun contract ->
        Contract.getCost contract command data (Some returnAddress) contractWallet txSkeleton
        |> Result.map (Log.info "Running contract with cost: %A")
        |> Result.mapError (Log.info "Error getting contract with cost: %A")
        |> ignore

        let rec run contract command data returnAddress contractWallet txSkeleton witnesses =
            Contract.run contract command data (Some returnAddress) contractWallet txSkeleton
            |> Result.bind (fun (tx, message) ->
                TxSkeleton.checkPrefix txSkeleton tx
                |> Result.bind (fun finalTxSkeleton ->
                    let witnesses = List.add (TxSkeleton.getContractWitness contract.hash command data returnAddress txSkeleton finalTxSkeleton) witnesses

                    match message with
                    | Some {cHash=cHash; command=command; data=data} ->
                        checkACS cHash
                        |> Result.bind (fun contract ->
                            run contract command data returnAddress contractWallet finalTxSkeleton witnesses
                        )
                    | None ->
                        Ok (finalTxSkeleton, witnesses)
                )
            )

        run contract command data returnAddress contractWallet txSkeleton []
        |> Result.map (fun (finalTxSkeleton, witnesses) ->
            Transaction.fromTxSkeleton finalTxSkeleton
            |> Transaction.addWitnesses witnesses
        )
    )
