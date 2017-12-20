module Blockchain.TransactionHandler

open Blockchain.EffectsWriter
open Messaging.Events
open Infrastructure
open Consensus
open Consensus.Transaction

let private handleContractActivationTransaction acs (tx : Types.Transaction) =
    match tx.contract with
        | Some code ->
            match Contract.compile code with
                | Ok (cHash, contract) -> 
                    ActiveContractSet.add cHash contract acs
                | Error err ->
                    Log.info "handle contract error: %A" err
                    acs
        | None -> 
            acs

let validateOrphanTransaction (utxoSet, mempool, orphanPool, acs) txHash tx  =                                 
    effectsWriter {            
        match Transaction.validateInputs utxoSet txHash tx with
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
        
let validateTransaction tx (utxoSet, mempool, orphanPool, acs) =
    effectsWriter {
        let txHash = Transaction.hash tx
                            
        if MemPool.containsTransaction txHash mempool || OrphanPool.containsTransaction txHash orphanPool then                
            return (utxoSet,mempool, orphanPool, acs) // Nothing to do, already in mempool
        else 
            match Transaction.validateBasic tx with
            | Error error ->                        
                Log.info "Transacation %s failed basic validation: %A" (Hash.toString txHash) error
                
                return (utxoSet, mempool, orphanPool, acs)
            | Ok tx ->
                match Transaction.validateInputs utxoSet txHash tx with   
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