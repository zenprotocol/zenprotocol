module Blockchain.Handler 

open Infrastructure
open Infrastructure.Writer
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus
open Blockchain
open Blockchain.EffectsWriter

let handleCommand command (utxoSet, mempool, orphanPool, acs) =   
    match command with
    | ValidateTransaction tx -> TransactionHandler.validateTransaction tx (utxoSet, mempool, orphanPool, acs)
    | GetMemPool peerId ->        
        effectsWriter {
            let txHashes = MemPool.getTxHashes mempool
            do! sendMemPool peerId txHashes
            return (utxoSet, mempool, orphanPool, acs)
        }
    | GetTransaction (peerId, txHash) -> 
         effectsWriter {
            match MemPool.getTransaction txHash mempool with
            | Some tx ->
                do! sendTransaction peerId tx
                return (utxoSet, mempool, orphanPool, acs)
            | None -> 
                return (utxoSet, mempool, orphanPool, acs)                                            
         } 
    | HandleMemPool (peerId,txHashes) ->
        let handleTxHash txHash =
            effectsWriter {
                if not (MemPool.containsTransaction txHash mempool) then
                    do! getTransaction peerId txHash
                else
                    return ()                    
            }
    
        let writer = 
            List.map handleTxHash txHashes
            |> List.fold (fun state writer -> Writer.bind state (fun () -> writer)) (Writer.ret ())
            
        Writer.bind writer (fun () -> Writer.ret (utxoSet, mempool, orphanPool, acs))            
                   
                                                     
let handleRequest request reply (utxoSet, mempool, orphanPool, acs) = 
    ret (utxoSet,mempool, orphanPool, acs)
    
let handleEvent event (utxoSet, mempool, orphanPool, acs) = 
    ret (utxoSet,mempool, orphanPool, acs)
    