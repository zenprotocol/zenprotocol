module Blockchain.Handler 

open Infrastructure
open Infrastructure.Writer
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus
open Blockchain
open Blockchain.EffectsWriter

let handleCommand command (utxoSet, mempool) =
    effectsWriter 
        {
            match command with
            | ValidateTransaction tx ->
                let txHash = Transaction.hash tx
            
                match MemPool.containsTransaction txHash mempool with
                | true -> return (utxoSet,mempool) // Nothing to do, already in mempool
                | false -> 
                    match Transaction.validateInputs utxoSet tx with
                    | Ok tx ->
                                                                        
                        let utxoSet = UtxoSet.handleTransaction txHash tx utxoSet
                        let mempool = MemPool.add txHash tx mempool
                        
                        do! publish (TransactionAddedToMemPool (txHash,tx))
                        
                        Log.info "Transaction %s added to mempool" (Hash.toString txHash)
                        
                        return (utxoSet,mempool)
                    | Error error -> 
                        // TODO: we should do something with the error here, like writing to log
                        // and banning peer?
                        // TODO: check if should be added to orphan list
                        return (utxoSet,mempool)                                             
        }
        
let handleRequest request reply (utxoSet, mempool) = 
    ret (utxoSet,mempool)
    
let handleEvent event (utxoSet, mempool) = 
    ret (utxoSet,mempool)
    