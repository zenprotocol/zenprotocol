module Blockchain.TransactionHandler

open Blockchain.EffectsWriter
open Messaging.Events
open Infrastructure
open Infrastructure.Writer
open Consensus
open Consensus.Transaction

let validateOrphanTransaction (utxoSet, mempool, orphanPool) txHash tx  =                                 
    effectsWriter {            
        match Transaction.validateInputs utxoSet txHash tx with
        | Ok tx ->
            let utxoSet = UtxoSet.handleTransaction txHash tx utxoSet
            let mempool = MemPool.add txHash tx mempool
            
            do! publish (TransactionAddedToMemPool (txHash,tx))
            Log.info "Orphan transaction %s added to mempool" (Hash.toString txHash)
            
            let orphanPool = OrphanPool.remove txHash orphanPool
            return utxoSet, mempool, orphanPool
        | Error (Orphan _) ->
            // transacation is still orphan, nothing to do
            return utxoSet, mempool, orphanPool            
        | Error error ->
            Log.info "Orphan transacation %s failed validation: %A" (Hash.toString txHash) error
        
            let orphanPool = OrphanPool.remove txHash orphanPool
            return utxoSet, mempool, orphanPool
                     
    }
        
let validateTransaction tx (utxoSet, mempool, orphanPool) =
    effectsWriter {
        let txHash = Transaction.hash tx
                            
        if MemPool.containsTransaction txHash mempool || OrphanPool.containsTransaction txHash orphanPool then                
            return (utxoSet,mempool, orphanPool) // Nothing to do, already in mempool
        else 
            match Transaction.validateBasic tx with
            | Error error ->                        
                Log.info "Transacation %s failed basic validation: %A" (Hash.toString txHash) error
                
                return (utxoSet,mempool, orphanPool)
            | Ok tx ->
                match Transaction.validateInputs utxoSet txHash tx with   
                | Error (Orphan tx) ->                                                                                                         
                    let orphanPool = OrphanPool.add txHash tx orphanPool
                    
                    Log.info "Transaction %s is orphan, adding to orphan pool" (Hash.toString txHash)                                                
                                        
                    return (utxoSet,mempool, orphanPool)               
                | Error error ->
                     Log.info "Transacation %s failed inputs validation: %A" (Hash.toString txHash) error
                             
                     return (utxoSet,mempool, orphanPool)                 
                | Ok tx ->                                                                        
                    let utxoSet = UtxoSet.handleTransaction txHash tx utxoSet
                    let mempool = MemPool.add txHash tx mempool
                    
                    do! publish (TransactionAddedToMemPool (txHash,tx))
                    
                    Log.info "Transaction %s added to mempool" (Hash.toString txHash)
                    
                    return! OrphanPool.foldWriter validateOrphanTransaction (utxoSet, mempool, orphanPool) orphanPool
    }