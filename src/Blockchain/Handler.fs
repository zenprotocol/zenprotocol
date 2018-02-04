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
open DatabaseContext

let handleCommand chain command session timestamp (state:State) =
    let contractPath = session.context.contractPath
   
    match command with
    | ValidateTransaction tx -> 
        effectsWriter {
            let! memoryState = TransactionHandler.validateTransaction chain session contractPath tx state.memoryState
            return {state with memoryState = memoryState}
        }
    | RequestMemPool peerId ->        
        effectsWriter {
            let txHashes = MemPool.getTxHashes state.memoryState.mempool
            do! sendMemPool peerId txHashes
            return state
        }
    | RequestTransaction (peerId, txHash) -> 
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
        BlockHandler.validateBlock chain contractPath session timestamp block false state
    | ValidateMinedBlock block -> 
        BlockHandler.validateBlock chain contractPath session timestamp block true state 
    | HandleTip header ->
        BlockHandler.handleTip chain session header state
    | ValidateNewBlockHeader (peerId, header) ->
        BlockHandler.handleNewBlockHeader chain session peerId header state
    | RequestTip peerId ->
        effectsWriter {
            if state.tipState.tip <> ExtendedBlockHeader.empty then
                do! sendTip peerId state.tipState.tip.header
            
            return state                                                      
        } 
    | RequestBlock (peerId, blockHash) ->
        effectsWriter {
            match BlockRepository.tryGetHeader session blockHash with
            | None -> return state
            | Some block -> 
                let block = BlockRepository.getFullBlock session block
                do! sendBlock peerId block
                                
                return state          
        }

let private selectTransactions _ = id
                                                            
let handleRequest (requestId:RequestId) request session timestamp state =
    match request with
    | ExecuteContract (cHash, command, returnAddress, txSkeleton) ->
        TransactionHandler.executeContract session txSkeleton cHash command returnAddress state.memoryState
        |> requestId.reply
    | GetBlockTemplate ->
        if MemPool.isEmpty state.memoryState.mempool || state.tipState.tip = ExtendedBlockHeader.empty then 
            requestId.reply<Types.Block option> None
        else
            let transactions = state.memoryState.mempool |> MemPool.getTransactions |> selectTransactions session
            let block = Block.createTemplate state.tipState.tip.header (Timestamp.now ()) state.tipState.ema state.memoryState.activeContractSet transactions
            
            requestId.reply<Types.Block option> (Some block)
    | GetBlock blockHash -> 
        match BlockRepository.tryGetHeader session blockHash with
        | Some header ->
            BlockRepository.getFullBlock session header
            |> Some 
            |> requestId.reply<Types.Block option>  
        | None -> requestId.reply<Types.Block option> None
    | GetBlockHeader blockHash -> 
        match BlockRepository.tryGetHeader session blockHash with
        | Some header ->            
            Some header.header
            |> requestId.reply<Types.BlockHeader option>   
        | None -> requestId.reply<Types.BlockHeader option> None
    | GetTip -> 
        if state.tipState.tip <> ExtendedBlockHeader.empty then
            Some (state.tipState.tip.hash, state.tipState.tip.header)            
            |> requestId.reply<(Hash.Hash*Types.BlockHeader) option>                 
        else 
            requestId.reply<(Hash.Hash*Types.BlockHeader) option> None                       
    ret state
        
let handleEvent event session timestamp state = 
    ret state
