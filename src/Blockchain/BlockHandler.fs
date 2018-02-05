module Blockchain.BlockHandler

open Blockchain
open Blockchain
open Blockchain
open Blockchain
open Blockchain
open Blockchain
open Blockchain
open Blockchain
open Consensus
open Infrastructure
open Blockchain.EffectsWriter
open Messaging.Events
open State

let getUTXO = UtxoSetRepository.get
let getOutput = TransactionRepository.getOutput

// Change the status of the entire chain from the root orphan block up to all tips from Orphan to Connected 
let rec private unorphanChain session extendedHeader =
    let unorphanChildChain parent extendedHeader =         
        let extendedHeader = 
            ExtendedBlockHeader.unorphan extendedHeader (Block.getChainWork (ExtendedBlockHeader.chainWork parent) extendedHeader.header)                     
        
        BlockRepository.saveHeader session extendedHeader
        
        unorphanChain session extendedHeader
                             
    let children =  BlockRepository.getBlockChildren session extendedHeader
  
    Seq.iter (fun child -> unorphanChildChain extendedHeader child) children    
        
        
// find chains that are longer than the minChainWork
// this is not final as we also need to check if the chain is connectable         
let rec private findLongerChains session extendedHeader minChainWork = 
    let children = BlockRepository.getBlockChildren session extendedHeader
    
    if Seq.isEmpty children then     
        if ExtendedBlockHeader.chainWork extendedHeader > minChainWork then [extendedHeader] else []
    else
        Seq.fold (fun chains child ->
            let chains' = findLongerChains session child minChainWork
            chains @ chains'
            |> List.sortByDescending ExtendedBlockHeader.chainWork 
            ) [] children
            
// Connect the entire chain, returning the valid tip along with state            
let rec private connectChain chain contractPath timestamp session (origin:ExtendedBlockHeader.T) originState (tip:ExtendedBlockHeader.T) =           
    if origin.header = tip.header then
        tip,originState
    else 
        let parent = BlockRepository.getHeader session tip.header.parent
    
        let validTip,(utxoSet, acs, ema, mempool) = 
            connectChain chain contractPath timestamp session origin originState parent
            
        // If invalid skip the block            
        if not (ExtendedBlockHeader.isValid tip) then
            validTip,(utxoSet,acs,ema, mempool)               
        elif validTip <> parent then
            // parent block is invalid, so so are we       
            BlockRepository.saveHeader session (ExtendedBlockHeader.invalid tip)         
                                                
            validTip,(utxoSet,acs,ema, mempool)                
        else 
            let block = BlockRepository.getFullBlock session tip        
        
            match Block.connect chain (getUTXO session) contractPath parent.header timestamp utxoSet acs ema block with
            | Error error ->
                BlockRepository.saveHeader session (ExtendedBlockHeader.invalid tip)
            
                Log.info "Failed connecting block %A due to %A" tip.hash error
                validTip,(utxoSet,acs,ema, mempool)
            | Ok (block,utxoSet,acs,ema) ->   
                
                BlockRepository.saveBlockState session tip.hash acs ema                          
                                                                                     
                let mempool = MemPool.handleBlock block mempool
                
                tip,(utxoSet,acs,ema, mempool)

let rec private connectLongestChain chain contractPath timestamp session origin originState chains minChainWork =
    let connectChain tip (best,state) =         
        // We are checking twice if current is longer than the bestChain, once before connecting and once after
        if ExtendedBlockHeader.chainWork tip > ExtendedBlockHeader.chainWork best then    
            let chain,state = connectChain chain contractPath timestamp session origin originState tip     
            
            if ExtendedBlockHeader.chainWork chain > ExtendedBlockHeader.chainWork best then
                chain,state
            else 
                best,state
        else 
            best,state
                
    let best,bestState = 
        List.fold (fun (best,state) chain ->connectChain chain (best,state)) (origin,originState) chains
            
    if (ExtendedBlockHeader.chainWork best) > minChainWork then
        (Some (best, bestState))
    else
        None
    

// Find the fork block of two chains
let rec private findForkBlock session (tip1:ExtendedBlockHeader.T) (tip2:ExtendedBlockHeader.T) = 
    if tip1.header = tip2.header then
        tip1
    else
        if tip1.header.blockNumber > tip2.header.blockNumber then
            let tip1 = BlockRepository.getHeader session tip1.header.parent
            findForkBlock session tip1 tip2
        else if tip2.header.blockNumber > tip1.header.blockNumber then
            let tip2 = BlockRepository.getHeader session tip2.header.parent
            findForkBlock session tip1 tip2
        else 
            let tip1 = BlockRepository.getHeader session tip1.header.parent
            let tip2 = BlockRepository.getHeader session tip2.header.parent
            findForkBlock session tip1 tip2
            
// Publish BlockRemoved events for a chain from tip to fork block           
let rec private removeBlocks session (forkBlock:ExtendedBlockHeader.T) (tip:ExtendedBlockHeader.T) = 
    if tip.header = forkBlock.header then 
        Writer.ret ()
    else 
        effectsWriter {
            let parent = BlockRepository.getHeader session tip.header.parent
            
            do! removeBlocks session forkBlock parent
            
            // Change status of the header from main to connected
            ExtendedBlockHeader.unmarkAsMain tip
            |> BlockRepository.saveHeader session
            
            let fullBlock = BlockRepository.getFullBlock session tip            
            do! publish (BlockRemoved (tip.hash, fullBlock))
            
            return ()
        }

// Publish BlockAdded events for a chain from tip to fork block            
let rec private addBlocks session (forkBlock:ExtendedBlockHeader.T) (tip:ExtendedBlockHeader.T) =
    if tip.header = forkBlock.header then 
        Writer.ret ()
    else 
        effectsWriter {
            let parent = BlockRepository.getHeader session tip.header.parent
            
            do! addBlocks session forkBlock parent
            
            // Change status of the header to main chain
            ExtendedBlockHeader.markAsMain tip
            |> BlockRepository.saveHeader session
            
            let fullBlock = BlockRepository.getFullBlock session tip
            do! publish (BlockAdded (tip.hash, fullBlock))                        
            
            return ()
        }

// Undo blocks from current state in order to get the state of the forkblock
let rec private undoBlocks session (forkBlock:ExtendedBlockHeader.T) (tip:ExtendedBlockHeader.T) utxoSet mempool = 
    if tip.header = forkBlock.header then
        let acs,ema = BlockRepository.getBlockState session tip.hash
    
        (utxoSet,acs,ema,mempool)
    else 
        let parent = BlockRepository.getHeader session tip.header.parent
        let fullBlock = BlockRepository.getFullBlock session tip
        
        let mempool = MemPool.undoBlock fullBlock mempool
        let utxoSet = 
            UtxoSet.undoBlock
                (getOutput session)
                (getUTXO session)
                fullBlock utxoSet
                              
        undoBlocks session forkBlock parent utxoSet mempool                       
        
// After applying block or blocks we must readd mempool transactions to the ACS and UTXO    
let getMemoryState chain session contractPath mempool orphanPool acs =                                                
    // We start with an empty mempool and current orphan pool
    // We first validate all orphan transactions according to the new state
    // We loop through existing mempool and adding it as we go to a new mempool
    // We don't publish AddedMemPool event for mempool transaction as they are already in mempool
    // We only publish add to mem pool transactions to orphan transactions
    let memoryState = {utxoSet=UtxoSet.asDatabase;
                        activeContractSet=acs;mempool=MemPool.empty;orphanPool=orphanPool}
    
    let memoryState = TransactionHandler.validateOrphanTransactions session contractPath memoryState

    Map.fold (fun writer txHash tx -> 
                      
        Writer.bind writer (fun memoryState ->
            TransactionHandler.validateInputs session contractPath txHash tx memoryState false)) memoryState mempool
                                            
let rollForwardChain chain contractPath timestamp state session block persistentBlock acs ema =
    effectsWriter {   
        // unorphan any orphan chain starting with current block
        unorphanChain session persistentBlock
       
        let mempool = MemPool.handleBlock block state.memoryState.mempool            
                                                                                     
        // find all longer chain and connect the longest, chains are ordered
        let currentChainWork = ExtendedBlockHeader.chainWork persistentBlock
        let chains = findLongerChains session persistentBlock currentChainWork
        let chain' = 
            connectLongestChain chain contractPath timestamp session persistentBlock 
                (UtxoSet.asDatabase,acs,ema, mempool) chains currentChainWork                                 
        
        match chain' with
        | Some (tip,(utxoSet,acs,ema,mempool)) -> 
            Log.info "Rolling forward chain to #%d %A" tip.header.blockNumber tip.hash
       
            let tip = ExtendedBlockHeader.markAsMain tip
        
            // update tip
            BlockRepository.updateTip session tip.hash
            
            UtxoSetRepository.save session utxoSet                        
                                                  
            let tipState = {activeContractSet=acs;ema=ema;tip=tip}
            let! memoryState = getMemoryState chain session contractPath mempool state.memoryState.orphanPool acs                                                                                           
                                                                                     
            do! addBlocks session persistentBlock tip
            return {state with tipState=tipState;memoryState=memoryState}
        | None ->
            let tipState = {activeContractSet=acs;ema=ema;tip=persistentBlock}
            let! memoryState = getMemoryState chain session contractPath mempool state.memoryState.orphanPool acs       
                                                                     
            return {state with tipState=tipState;memoryState=memoryState}
    }

let private handleGenesisBlock chain contractPath session timestamp (state:State) blockHash block =
    effectsWriter {                          
        match Block.connect chain (getUTXO session) contractPath Block.genesisParent timestamp UtxoSet.asDatabase 
                state.tipState.activeContractSet state.tipState.ema block with
        | Error error ->
            Log.info "Failed connecting genesis block %A due to %A" (Block.hash block) error
            return state
        | Ok (block,utxoSet,acs,ema) ->
            Log.info "Genesis block received" 
          
            let extendedHeader = ExtendedBlockHeader.createGenesis blockHash block
            BlockRepository.saveHeader session extendedHeader
            BlockRepository.saveFullBlock session blockHash block
            BlockRepository.saveBlockState session blockHash acs ema
            BlockRepository.updateTip session extendedHeader.hash
            
            UtxoSetRepository.save session utxoSet                        
                           
            // Pulishing event of the new block
            do! publish (BlockAdded (blockHash, block))
                                                            
            return! rollForwardChain chain contractPath timestamp state session block extendedHeader acs ema               
    }

// Handle new block that is extending the main chain
// New block can also unorphan a chain and extend the chain even further
// So we also have to unorphan any chain and find longest chain
let private handleMainChain chain contractPath session timestamp (state:State) (parent:ExtendedBlockHeader.T) blockHash block =    
    effectsWriter {            
        match Block.connect chain (getUTXO session) contractPath parent.header timestamp UtxoSet.asDatabase 
                state.tipState.activeContractSet state.tipState.ema block with
        | Error error ->
            Log.info "Failed connecting block %A due to %A" (Block.hash block) error
            return state
        | Ok (block,utxoSet,acs,ema) ->  
            Log.info "New block #%d %A" block.header.blockNumber blockHash 
                                     
            let extendedHeader = ExtendedBlockHeader.createMain parent blockHash block
                                    
            BlockRepository.saveHeader session extendedHeader
            BlockRepository.saveFullBlock session blockHash block
            BlockRepository.saveBlockState session extendedHeader.hash acs ema
            BlockRepository.updateTip session extendedHeader.hash
            
            UtxoSetRepository.save session utxoSet                        
                                                            
            // Pulishing event of the new block
            do! publish (BlockAdded (blockHash, block))

            return! rollForwardChain chain contractPath timestamp state session block extendedHeader acs ema
    }                   

// New block that is extending one of the fork chains
// We first unorphan any chain and look for any chain that is longer than main chain
// We then try to connect any of the longer chain, and check that the connected chain is actually longer
// Finally we raise events that remove blocks from main chain up to the fork block and raise add events for all new blocks    
let private handleForkChain chain contractPath session timestamp (state:State) parent blockHash block =
    effectsWriter {     
        let extendedHeader = ExtendedBlockHeader.createConnected parent blockHash block
        BlockRepository.saveHeader session extendedHeader
        BlockRepository.saveFullBlock session blockHash block
    
        // unorphan any orphan chain starting with current block
        unorphanChain session extendedHeader
    
        let currentTip = state.tipState.tip
        let currentChainWork = ExtendedBlockHeader.chainWork currentTip
        
        match findLongerChains session extendedHeader currentChainWork with
        | [] -> 
            // No chain is longer than current chain
            return state
        | chains ->                                                         
            let forkBlock = findForkBlock session extendedHeader currentTip
                
            // undo blocks from mainnet to get fork block state
            let forkState = undoBlocks session forkBlock currentTip 
                                UtxoSet.asDatabase state.memoryState.mempool 
                         
            match connectLongestChain chain contractPath timestamp session 
                    forkBlock forkState chains currentChainWork with
            | None ->
                // No connectable chain is not longer than current chain
                return state                                    
            | Some (tip,(utxoSet,acs,ema,mempool)) ->
                Log.info "Reorg to #%d %A" tip.header.blockNumber tip.hash
            
                let tip = ExtendedBlockHeader.markAsMain tip
            
                // update tip
                BlockRepository.updateTip session tip.hash 
                UtxoSetRepository.save session utxoSet                        
                
                do! removeBlocks session forkBlock currentTip
                do! addBlocks session forkBlock tip
                
                let tipState = {activeContractSet=acs;ema=ema;tip=tip}
                let! memoryState = getMemoryState chain session contractPath mempool state.memoryState.orphanPool acs  
                
                return {state with tipState=tipState;memoryState=memoryState}
    }     
        
let validateBlock chain contractPath session timestamp block mined (state:State) =
    effectsWriter { 
        let blockHash = Block.hash block
        
        let blockRequest = Map.tryFind blockHash state.blockRequests
        
        let blockRequests = Map.remove blockHash state.blockRequests
        let state = {state with blockRequests = blockRequests}
        
        Log.info "Validating new block #%d %A" block.header.blockNumber blockHash
    
        // checking if block already exist
        if BlockRepository.contains session blockHash then
            // nothing to do, blocks already exist
            return state
        else
        match Block.validate chain block with
        | Error error ->
            Log.info "Block %A failed validation due to %A" blockHash error
            return state
        | Ok block ->
            if blockRequest = Some NewBlock || mined then
                Log.info "Publishing new block %A" blockHash
                do! publishBlock block.header
                
            if Block.isGenesis chain block then
                let! state' = handleGenesisBlock chain contractPath session timestamp state blockHash block
                 
                if state'.tipState.tip <> state.tipState.tip then
                    do! publish (TipChanged state'.tipState.tip.header)
                         
                return state'
            else
                // try to find parent block
                match BlockRepository.tryGetHeader session block.header.parent with
                | None ->
                    Log.info "Adding block as orphan #%d %A" block.header.blockNumber blockHash
                    
                    let extendedHeader = ExtendedBlockHeader.createOrphan blockHash block
                    BlockRepository.saveHeader session extendedHeader
                    BlockRepository.saveFullBlock session blockHash block
                    
                    if not <| Map.containsKey block.header.parent state.blockRequests then
                        // asking network for the parent block
                        let blockRequests = Map.add block.header.parent ParentBlock state.blockRequests
                        do! getBlock block.header.parent
                        
                        return {state with blockRequests=blockRequests}
                    else
                        return state
                | Some parent ->
                    match ExtendedBlockHeader.status parent with
                    | ExtendedBlockHeader.Invalid ->
                        // Ignoring the block
                        return state
                    | ExtendedBlockHeader.Orphan ->
                        let extendedHeader = ExtendedBlockHeader.createOrphan blockHash block
                        BlockRepository.saveHeader session extendedHeader
                        BlockRepository.saveFullBlock session blockHash block

                        return state
                    | ExtendedBlockHeader.MainChain
                    | ExtendedBlockHeader.Connected ->
                        let! state' =
                            if parent.hash = state.tipState.tip.hash then
                                handleMainChain chain contractPath session timestamp state parent blockHash block
                            else
                                handleForkChain chain contractPath session timestamp state parent blockHash block
                                
                        if state'.tipState.tip <> state.tipState.tip then
                            do! publish (TipChanged state'.tipState.tip.header)

                        return state'
    }

let handleNewBlockHeader chain session peerId header (state:State) =
    effectsWriter {            
        let blockHash = BlockHeader.hash header
       
        if (not <| BlockRepository.contains session blockHash) && (not <| Map.containsKey blockHash state.blockRequests) then
            match BlockHeader.validate chain header with
            | Ok header ->
                
                Log.info "Request new block #%d %A from peers" header.blockNumber blockHash
                
                // TODO: mark request as request for new block so peer cannot trick as to think it is not a new block
                let blockRequests = Map.add blockHash NewBlock state.blockRequests
            
                do! getNewBlock peerId blockHash
                
                return {state with blockRequests = blockRequests}
            | Error _ -> 
                return state
        else 
            return state
    }

let handleTip chain session header (state:State) =
    effectsWriter {
        let blockHash = BlockHeader.hash header                       
    
        if (not <| BlockRepository.contains session blockHash) && (not <| Map.containsKey blockHash state.blockRequests) then
            match BlockHeader.validate chain header with
            | Ok header ->
                
                Log.info "Request block #%d %A from peers" header.blockNumber blockHash
            
                let blockRequests = Map.add blockHash Tip state.blockRequests
            
                do! getBlock blockHash
                
                return {state with blockRequests = blockRequests}
            | Error _ -> 
                return state
        else 
            return state
    }