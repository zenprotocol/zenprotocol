module Blockchain.Handler 

open Infrastructure
open Infrastructure.Writer
open Infrastructure.ServiceBus.Agent
open Messaging.Services
open Messaging.Events
open Consensus
open Blockchain
open Blockchain.EffectsWriter
open State

let handleCommand chain command timestamp (state:State) =   
    match command with
    | ValidateTransaction tx -> 
        effectsWriter {
            let! memoryState = TransactionHandler.validateTransaction chain tx state.memoryState
            return {state with memoryState = memoryState}
        }
    | GetMemPool peerId ->        
        effectsWriter {
            let txHashes = MemPool.getTxHashes state.memoryState.mempool
            do! sendMemPool peerId txHashes
            return state
        }
    | GetTransaction (peerId, txHash) -> 
         effectsWriter {
            match MemPool.getTransaction txHash state.memoryState.mempool with
            | Some tx ->
                do! sendTransaction peerId tx
                return state
            | None -> 
                return state                                          
         } 
    | HandleMemPool (peerId,txHashes) ->
        let handleTxHash txHash =
            effectsWriter {
                Log.info "Handling mempool message"
            
                if not (MemPool.containsTransaction txHash state.memoryState.mempool) then
                    do! getTransaction peerId txHash
                else
                    return ()                    
            }
    
        let writer = 
            List.map handleTxHash txHashes
            |> List.fold (fun state writer -> Writer.bind state (fun () -> writer)) (Writer.ret ())
            
        Writer.bind writer (fun () -> Writer.ret state)    
    | ValidateBlock block ->
        BlockHandler.validateBlock chain timestamp block false state
    | ValidateMinedBlock block -> 
        BlockHandler.validateBlock chain timestamp block true state 
    | HandleTip header ->
        BlockHandler.handleTip chain header state
    | ValidateNewBlockHeader (peerId, header) ->
        BlockHandler.handleNewBlockHeader chain peerId header state
    | GetTip peerId ->
        effectsWriter {
            if state.tipState.tip <> PersistentBlock.empty then
                do! sendTip peerId state.tipState.tip.header
            
            return state                                                      
        } 
    | GetBlock (peerId, blockHash) ->
        effectsWriter {
            match BlockRepository.tryFind state.blockRepository blockHash with
            | None -> return state
            | Some block -> 
                let block = PersistentBlock.fetchBlock block
                do! sendBlock peerId block
                                
                return state          
        }
                                                    
let handleRequest reply request timestamp state = 
    match request with
    | ExecuteContract (txSkeleton, cHash) ->
        TransactionHandler.executeContract txSkeleton cHash state.memoryState
        |> Result.map Transaction.fromTxSkeleton
        |> function 
        | Result.Ok tx -> Ok tx
        | Result.Error err -> Error err
        |> reply
    | _ -> ()
    ret state
    
let handleEvent event timestamp state = 
    ret state
