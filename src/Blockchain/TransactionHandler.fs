module Blockchain.TransactionHandler

open Blockchain.EffectsWriter
open Messaging.Events
open Infrastructure
open Consensus
open Consensus.Transaction
open Consensus.TransactionValidation
open State
open Infrastructure.ServiceBus.Agent

let private activateContract chain contractPath acs (tx : Types.Transaction) shouldPublishEvents =
    effectsWriter
        {
            match tx.contract with
            | Some code ->
                match Contract.compile contractPath code with
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

let private validateOrphanTransaction chain contractPath state txHash tx  =
    effectsWriter 
        {
            match TransactionValidation.validateInputs state.activeContractSet state.utxoSet txHash tx with
            | Ok tx ->
                let utxoSet = UtxoSet.handleTransaction txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool

                do! publish (TransactionAddedToMemPool (txHash, tx))
                Log.info "Orphan transaction %s added to mempool" (Hash.toString txHash)

                let! acs = activateContract chain contractPath state.activeContractSet tx true

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
        
let rec validateOrphanTransactions chain contractPath state =      
    effectsWriter {
        let! state' = OrphanPool.foldWriter (validateOrphanTransaction chain contractPath) state state.orphanPool
        
        // if orphan pool changed we run again until there is no change
        if state.orphanPool <> state.orphanPool then
            return! validateOrphanTransactions chain contractPath state'
        else
            return state'
    }
          
let validateInputs chain contractPath txHash tx (state:MemoryState) shouldPublishEvents =
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
                let! acs = activateContract chain contractPath state.activeContractSet tx shouldPublishEvents

                let utxoSet = UtxoSet.handleTransaction txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool

                if shouldPublishEvents then
                    do! publish (TransactionAddedToMemPool (txHash,tx))

                Log.info "Transaction %s added to mempool" (Hash.toString txHash)

                let state = {state with activeContractSet=acs;mempool=mempool;utxoSet=utxoSet}

                
                return! validateOrphanTransactions chain contractPath state
        }


let validateTransaction chain contractPath tx (state:MemoryState) =
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
                return! validateInputs chain contractPath txHash tx state true
    }

let executeContract txSkeleton cHash state =
    match ActiveContractSet.tryFind cHash state.activeContractSet with
    | Some contract ->
        Contract.run contract txSkeleton
        |> Result.bind (TxSkeleton.checkPrefix txSkeleton)
        |> Result.map (Transaction.fromTxSkeleton contract.hash)
        |> Result.map (Transaction.addContractWitness contract.hash txSkeleton)
    | None -> Error "Contract not active"