module Blockchain.TransactionHandler

open Blockchain.EffectsWriter
open Messaging.Events
open Infrastructure
open Consensus
open Consensus.Transaction
open Consensus.TransactionValidation

let private handleContractActivationTransaction acs (tx : Types.Transaction) =
    match tx.contract with
    | Some code ->
        match Contract.compile code with
        | Ok contract -> 
            Log.warning "activating contract: %A" (Hash.bytes contract.hash)
            ActiveContractSet.add contract.hash contract acs
        | Error err ->
            Log.info "handle contract error: %A" err
            acs
    | None -> 
        acs

let private validateOrphanTransaction (utxoSet, mempool, orphanPool, acs: ActiveContractSet.T) txHash tx  =                                 
    effectsWriter {            
        match TransactionValidation.validateInputs acs utxoSet txHash tx with
        | Ok tx ->
            let utxoSet = UtxoSet.handleTransaction txHash tx utxoSet
            let mempool = MemPool.add txHash tx mempool
            
            do! publish (TransactionAddedToMemPool (txHash,tx))
            Log.info "Orphan transaction %s added to mempool" (Hash.toString txHash)

            let acs = handleContractActivationTransaction acs tx

            let orphanPool = OrphanPool.remove txHash orphanPool
            return utxoSet, mempool, orphanPool, acs
        | Error (Orphan _) ->
            // transacation is still orphan, nothing to do
            return utxoSet, mempool, orphanPool, acs            
        | Error error ->
            Log.info "Orphan transacation %s failed validation: %A" (Hash.toString txHash) error
        
            let orphanPool = OrphanPool.remove txHash orphanPool
            return utxoSet, mempool, orphanPool, acs
                     
    }
        
let validateTransaction tx (utxoSet: Map<Types.Outpoint, UtxoSet.InputStatus>, mempool, orphanPool, acs:ActiveContractSet.T) =
    effectsWriter {
        let txHash = Transaction.hash tx
                            
        if MemPool.containsTransaction txHash mempool || OrphanPool.containsTransaction txHash orphanPool then                
            return (utxoSet,mempool, orphanPool, acs) // Nothing to do, already in mempool
        else 
            match TransactionValidation.validateBasic tx with
            | Error error ->                        
                Log.info "Transacation %s failed basic validation: %A" (Hash.toString txHash) error
                
                return (utxoSet, mempool, orphanPool, acs)
            | Ok tx ->
                match TransactionValidation.validateInputs acs utxoSet txHash tx with   
                | Error (Orphan tx) ->                                                                                                         
                    let orphanPool = OrphanPool.add txHash tx orphanPool
                    
                    Log.info "Transaction %s is orphan, adding to orphan pool" (Hash.toString txHash)                                                                           
                                        
                    return (utxoSet, mempool, orphanPool, acs)               
                | Error error ->
                     Log.info "Transacation %s failed inputs validation: %A" (Hash.toString txHash) error
                             
                     return (utxoSet, mempool, orphanPool, acs)                 
                | Ok tx ->
                    let acs = handleContractActivationTransaction acs tx

                    let utxoSet = UtxoSet.handleTransaction txHash tx utxoSet
                    let mempool = MemPool.add txHash tx mempool


                    do! publish (TransactionAddedToMemPool (txHash,tx))
                    
                    Log.info "Transaction %s added to mempool" (Hash.toString txHash)
                    
                    return! OrphanPool.foldWriter validateOrphanTransaction (utxoSet, mempool, orphanPool, acs) orphanPool
    }

let executeContract txSkeleton cHash reply (utxoSet, mempool, orphanPool, acs) =
    reply (
        match ActiveContractSet.tryFind cHash acs with
        | Some contract ->
            Contract.run contract txSkeleton
            |> Result.bind (TxSkeleton.checkPrefix txSkeleton)
        | None -> 
            Error "Contract not active"
    )

    Writer.ret (utxoSet, mempool, orphanPool, acs)