module Blockchain.TransactionHandler

open Blockchain.EffectsWriter
open Messaging.Events
open Infrastructure
open Consensus
open Consensus.Transaction
open Consensus.TransactionValidation
open State
open Infrastructure.ServiceBus.Agent

let private activateContract chain acs (tx : Types.Transaction) shouldPublishEvents =
    effectsWriter
        {
            match tx.contract with
            | Some code ->
                match Contract.compile code with
                | Ok contract ->
                    Log.warning "activating contract: %A" (Address.encode chain (Address.Contract contract.hash))

                    if shouldPublishEvents then
                        do! publish (ContractActivated (Contract.hash contract))

                    return ActiveContractSet.add contract.hash contract acs
                | Error err ->
                    Log.info "handle contract error: %A" err
                    return acs
            | None ->
                return acs
        }

let private validateOrphanTransaction chain state txHash tx  =
    effectsWriter 
        {
            match TransactionValidation.validateInputs state.activeContractSet state.utxoSet txHash tx with
            | Ok tx ->
                let utxoSet = UtxoSet.handleTransaction txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool

                do! publish (TransactionAddedToMemPool (txHash, tx))
                Log.info "Orphan transaction %s added to mempool" (Hash.toString txHash)

                let! acs = activateContract chain state.activeContractSet tx true

                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {state with activeContractSet=acs;mempool=mempool;utxoSet=utxoSet; orphanPool=orphanPool}
            | Error (Orphan _) ->
                // transacation is still orphan, nothing to do
                return state
            | Error error ->
                Log.info "Orphan transacation %s failed validation: %A" (Hash.toString txHash) error

                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {state with orphanPool = orphanPool}
        }

let validateInputs chain txHash tx (state:MemoryState) shouldPublishEvents =
    effectsWriter
        {
            match TransactionValidation.validateInputs state.activeContractSet state.utxoSet txHash tx with
            | Error Orphan ->
                let orphanPool = OrphanPool.add txHash tx state.orphanPool

                Log.info "Transaction %s is orphan, adding to orphan pool" (Hash.toString txHash)

                return {state with orphanPool = orphanPool}
            | Error error ->
                 Log.info "Transacation %s failed inputs validation: %A" (Hash.toString txHash) error

                 return state
            | Ok tx ->
                let! acs = activateContract chain state.activeContractSet tx shouldPublishEvents

                let utxoSet = UtxoSet.handleTransaction txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool

                if shouldPublishEvents then
                    do! publish (TransactionAddedToMemPool (txHash,tx))

                Log.info "Transaction %s added to mempool" (Hash.toString txHash)

                let state = {state with activeContractSet=acs;mempool=mempool;utxoSet=utxoSet}

                // TODO: going through the orphan pool once might be not enough if we have inter dependencies within the orphan pool
                return! OrphanPool.foldWriter (validateOrphanTransaction chain) state state.orphanPool
        }


let validateTransaction chain tx (state:MemoryState) =
    effectsWriter {
        let txHash = Transaction.hash tx

        if MemPool.containsTransaction txHash state.mempool || OrphanPool.containsTransaction txHash state.orphanPool then
            return state
        else
            match TransactionValidation.validateBasic tx with
            | Error error ->
                Log.info "Transacation %s failed basic validation: %A" (Hash.toString txHash) error

                return state
            | Ok tx ->
                return! validateInputs chain txHash tx state true
    }

let executeContract txSkeleton cHash state =
    match ActiveContractSet.tryFind cHash state.activeContractSet with
    | Some contract ->
        Contract.run contract state.utxoSet txSkeleton
        |> Result.bind (TxSkeleton.checkPrefix txSkeleton)
    | None -> Error "Contract not active"