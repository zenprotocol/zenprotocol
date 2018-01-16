module Blockchain.BlockHandler

open Blockchain
open Consensus
open Infrastructure
open Blockchain.EffectsWriter
open Messaging.Events
open State

// TODO: logging
// TODO: add blocks that fail basic validation as failed blocks (so we won't ask for them later)

// Mark the entire chain from the root orphan block up to all tips as unconnected
let rec private unorphanChain blockRepository block =
    let unorphanChildChain parent block () = 
        effectsWriter {
            let block = 
                PersistentBlock.unorphanBlock block (Block.getChainWork (PersistentBlock.chainWork parent) (PersistentBlock.header block))                     
            
            do! BlockRepository.update blockRepository block
            
            do! unorphanChain blockRepository block
            
            return ()
        }
         
    effectsWriter  {
        let children =  BlockRepository.getBlockChildren blockRepository block
  
        do! List.fold (fun writer child -> Writer.bind writer (unorphanChildChain block child)) (Writer.ret ()) children
    }    
        
// find chains that are longer than the minChainWork
// this is not final as we also need to check if the chain is connectable         
let rec private findLongerChains blockRepository block minChainWork = 
    match BlockRepository.getBlockChildren blockRepository block with
    | [] -> if PersistentBlock.chainWork block > minChainWork then [block] else []            
    | children ->
        List.fold (fun chains child ->
            let chains' = findLongerChains blockRepository child minChainWork
            chains @ chains'
            |> List.sortByDescending PersistentBlock.chainWork 
            ) [] children
            
// Connect the entire chain, returning the valid tip along with state            
let rec private connectChain chain timestamp blockRepository (origin:PersistentBlock.T) originState (tip:PersistentBlock.T) = 
    effectsWriter {        
        if origin.header = tip.header then
            return tip,originState
        else 
            let parent = BlockRepository.findParent blockRepository tip
        
            let! validTip,(utxoSet, acs, ema, mempool) = connectChain chain timestamp blockRepository origin originState parent
                
            // If invalid skip the block            
            if not (PersistentBlock.isValid tip) then
                return validTip,(utxoSet,acs,ema, mempool)               
            elif validTip <> parent then
                // parent block is invalid, so so are we       
                do! BlockRepository.update blockRepository (PersistentBlock.invalid tip)         
                                                    
                return validTip,(utxoSet,acs,ema, mempool)                
            else 
                let block = (PersistentBlock.fetchBlock tip)
            
                match Block.connect chain (PersistentBlock.header parent) timestamp utxoSet acs ema block with
                | Error error ->
                    do! BlockRepository.update blockRepository (PersistentBlock.invalid tip)
                
                    Log.info "Failed connecting block %A due to %A" (PersistentBlock.hash tip) error
                    return validTip,(utxoSet,acs,ema, mempool)
                | Ok (block,utxoSet,acs,ema) ->   
                    
                    do! BlockRepository.update blockRepository (PersistentBlock.connect tip block ema)         
                                                                                         
                    let mempool = MemPool.handleBlock block mempool
                    
                    return tip,(utxoSet,acs,ema, mempool)
    }

let rec private connectLongestChain chain timestamp blockRepository origin originState chains minChainWork =
    let connectChain tip (best,state) = 
        effectsWriter {
            // We are checking twice if current is longer than the bestChain, once before connecting and once after
            if PersistentBlock.chainWork tip > PersistentBlock.chainWork best then    
                let! chain,state = connectChain chain timestamp blockRepository origin originState tip     
                
                if PersistentBlock.chainWork chain > PersistentBlock.chainWork best then
                    return chain,state
                else 
                    return best,state
            else 
                return best,state
        }
        
    effectsWriter {
        let! best,bestState = List.fold (fun writer chain -> Writer.bind writer (connectChain chain)) (Writer.ret (origin,originState)) chains
                
        if (PersistentBlock.chainWork best) > minChainWork then
            return (Some (best, bestState))
        else
            return None
    }

// Find the fork block of two chains
let rec private findForkBlock blockRepository (tip1:PersistentBlock.T) (tip2:PersistentBlock.T) = 
    if tip1.header = tip2.header then
        tip1
    else
        let header1 = PersistentBlock.header tip1
        let header2 = PersistentBlock.header tip2 
        
        if header1.blockNumber > header2.blockNumber then
            let tip1 = BlockRepository.findParent blockRepository tip1
            findForkBlock blockRepository tip1 tip2
        else if header2.blockNumber > header1.blockNumber then
            let tip2 = BlockRepository.findParent blockRepository tip2
            findForkBlock blockRepository tip1 tip2
        else 
            let tip1 = BlockRepository.findParent blockRepository tip1
            let tip2 = BlockRepository.findParent blockRepository tip2
            findForkBlock blockRepository tip1 tip2
            
// Publish BlockRemoved events for a chain from tip to fork block           
let rec private removeBlocks blockRepository (forkBlock:PersistentBlock.T) (tip:PersistentBlock.T) = 
    if tip.header = forkBlock.header then 
        Writer.ret ()
    else 
        effectsWriter {
            let parent = BlockRepository.findParent blockRepository tip
            
            do! removeBlocks blockRepository forkBlock parent
            
            do! publish (BlockRemoved (PersistentBlock.fetchBlock tip))
            
            return ()
        }

// Publish BlockAdded events for a chain from tip to fork block            
let rec private addBlocks blockRepository (forkBlock:PersistentBlock.T) (tip:PersistentBlock.T) =
    if tip.header = forkBlock.header then 
        Writer.ret ()
    else 
        effectsWriter {
            let parent = BlockRepository.findParent blockRepository tip
            
            do! addBlocks blockRepository forkBlock parent
            
            do! publish (BlockAdded (PersistentBlock.fetchBlock tip))
            
            return ()
        }

// Undo blocks from current state in order to get the state of the forkblock            
let rec private undoBlocks blockRepository (forkBlock:PersistentBlock.T) (tip:PersistentBlock.T) (utxoSet,acs,ema,mempool) = 
    if tip.header = forkBlock.header then
        (utxoSet,acs,ema,mempool)
    else 
        let parent = BlockRepository.findParent blockRepository tip
        let block = PersistentBlock.fetchBlock tip
        
        let utxoSet = UtxoSet.undoBlock block utxoSet
        let acs = ActiveContractSet.undoBlock block acs
        let ema = PersistentBlock.ema parent
        let mempool = MemPool.undoBlock block mempool

        undoBlocks blockRepository forkBlock parent (utxoSet,acs,ema,mempool)                       
        
// After applying block or blocks we must readd mempool transactions to the ACS and UTXO    
let getMemoryState chain utxoSet mempool orphanPool acs =                                                
    // We start with an empty mempool and current orphan pool
    // We first validate all orphan transactions according to the new state
    // We loop through existing mempool and adding it as we go to a new mempool
    // We don't publish AddedMemPool event for mempool transaction as they are already in mempool
    // We only publish add to mem pool transactions to orphan transactions
    let memoryState = {utxoSet=utxoSet;activeContractSet=acs;mempool=MemPool.empty;orphanPool=orphanPool}
    
    let memoryState = TransactionHandler.validateOrphanTransactions chain memoryState

    Map.fold (fun writer txHash tx -> 
                      
        Writer.bind writer (fun memoryState ->
            TransactionHandler.validateInputs chain txHash tx memoryState false)) memoryState mempool
                                            
let rollForwardChain chain timestamp state block persistentBlock utxoSet acs ema =
    effectsWriter {   
        // unorphan any orphan chain starting with current block
        do! unorphanChain state.blockRepository persistentBlock
       
        let mempool = MemPool.handleBlock block state.memoryState.mempool            
                                                                                     
        // find all longer chain and connect the longest, chains are ordered
        let currentChainWork = PersistentBlock.chainWork persistentBlock
        let chains = findLongerChains state.blockRepository persistentBlock currentChainWork
        let! chain' = connectLongestChain chain timestamp state.blockRepository persistentBlock (utxoSet,acs,ema, mempool) chains currentChainWork                                 
        
        match chain' with
        | Some (tip,(utxoSet,acs,ema,mempool)) -> 
            Log.info "Rolling forward chain to #%d %A" tip.header.blockNumber tip.hash
        
            // Marking the new tip as tip and unmarking current tip
            do! BlockRepository.update state.blockRepository (PersistentBlock.untipBlock persistentBlock)
            do! BlockRepository.update state.blockRepository (PersistentBlock.markBlockAsTip tip)
                                      
            let tipState = {utxoSet=utxoSet;activeContractSet=acs;ema=ema;tip=tip}
            let! memoryState = getMemoryState chain utxoSet mempool state.memoryState.orphanPool acs                                                                                           
                                                                                     
            do! addBlocks state.blockRepository persistentBlock tip
            return {state with tipState=tipState;memoryState=memoryState}
        | None ->
            let tipState = {utxoSet=utxoSet;activeContractSet=acs;ema=ema;tip=persistentBlock}
            let! memoryState = getMemoryState chain utxoSet mempool state.memoryState.orphanPool acs       
                                                                     
            return {state with tipState=tipState;memoryState=memoryState}
    }

let private handleGenesisBlock chain timestamp (state:State) blockHash block =
    effectsWriter {           
        match Block.connect chain Block.genesisParent timestamp state.tipState.utxoSet state.tipState.activeContractSet state.tipState.ema block with
        | Error error ->
            Log.info "Failed connecting genesis block %A due to %A" (Block.hash block) error
            return state
        | Ok (block,utxoSet,acs,ema) ->
            Log.info "Genesis block received" 
          
            let persistentBlock = 
                PersistentBlock.createGenesis chain blockHash block ema
            
            // Pulishing event of the new block
            do! publish (BlockAdded block)
                                    
            do! BlockRepository.insert state.blockRepository persistentBlock
            
            return! rollForwardChain chain timestamp state block persistentBlock utxoSet acs ema               
    }

// Handle new block that is extending the main chain
// New block can also unorphan a chain and extend the chain even further
// So we also have to unorphan any chain and find longest chain
let private handleMainChain chain timestamp (state:State) parent blockHash block =    
    effectsWriter {    
        match Block.connect chain (PersistentBlock.header parent) timestamp state.tipState.utxoSet state.tipState.activeContractSet state.tipState.ema block with
        | Error error ->
            Log.info "Failed connecting block %A due to %A" (Block.hash block) error
            return state
        | Ok (block,utxoSet,acs,ema) ->  
            Log.info "New block #%d %A" block.header.blockNumber blockHash 
                                     
            let persistentBlock = PersistentBlock.createTip parent blockHash block ema
            
            // Pulishing event of the new block
            do! publish (BlockAdded block)
            
            // Removing tip status from parent and adding new block
            let parent = PersistentBlock.untipBlock parent
            
            do! BlockRepository.update state.blockRepository parent
            do! BlockRepository.insert state.blockRepository persistentBlock
        
            return! rollForwardChain chain timestamp state block persistentBlock utxoSet acs ema
    }                   

// New block that is extending one of the fork chains
// We first unorphan any chain and look for any chain that is longer than main chain
// We then try to connect any of the longer chain, and check that the connected chain is actually longer
// Finally we raise events that remove blocks from main chain up to the fork block and raise add events for all new blocks    
let private handleForkChain chain timestamp (state:State) parent blockHash block =
    effectsWriter {     
        let persistentBlock = PersistentBlock.createConnected parent blockHash block
        do! BlockRepository.insert state.blockRepository persistentBlock
    
        // unorphan any orphan chain starting with current block
        do! unorphanChain state.blockRepository persistentBlock
    
        let currentTip = state.tipState.tip
        let currentChainWork = PersistentBlock.chainWork currentTip
        
        match findLongerChains state.blockRepository persistentBlock currentChainWork with
        | [] -> 
            // No chain is longer than current chain
            return state
        | chains ->                                                         
            let forkBlock = findForkBlock state.blockRepository persistentBlock currentTip
                
            // undo blocks from mainnet to get fork block state
            let forkState = undoBlocks state.blockRepository forkBlock currentTip 
                                (state.tipState.utxoSet,state.tipState.activeContractSet,state.tipState.ema, state.memoryState.mempool)
            
            let! chain' = connectLongestChain chain timestamp state.blockRepository forkBlock forkState chains currentChainWork
            match chain' with
            | None ->
                // No connectable chain is not longer than current chain
                return state                                    
            | Some (tip,(utxoSet,acs,ema,mempool)) ->
                Log.info "Reorg to #%d %A" tip.header.blockNumber tip.hash
            
                // Marking the new tip as tip and unmarking main chain tip
                do! BlockRepository.update state.blockRepository (PersistentBlock.untipBlock currentTip)
                do! BlockRepository.update state.blockRepository (PersistentBlock.markBlockAsTip tip)
            
                do! removeBlocks state.blockRepository forkBlock currentTip
                do! addBlocks state.blockRepository forkBlock tip
                
                let tipState = {utxoSet=utxoSet;activeContractSet=acs;ema=ema;tip=tip}
                let! memoryState = getMemoryState chain utxoSet mempool state.memoryState.orphanPool acs  
                
                return {state with tipState=tipState;memoryState=memoryState}
    }     
        
let validateBlock chain timestamp block mined (state:State) =
    effectsWriter { 
        let blockHash = Block.hash block
        
        let blockRequest = Map.tryFind blockHash state.blockRequests 
        
        let blockRequests = Map.remove blockHash state.blockRequests
        let state = {state with blockRequests = blockRequests}
        
        Log.info "Validating new block #%d %A" block.header.blockNumber blockHash
    
        // checking if block already exist
        match BlockRepository.exist state.blockRepository blockHash with
        | true -> 
            // nothing to do, blocks already exist
            return state
        | false ->                                                        
            match Block.validate chain block with
            | Error error ->
                Log.info "Block %A failed validation due to %A" (Block.hash block) error
                return state
            | Ok block -> 
                if blockRequest = Some NewBlock || mined then                                  
                    do! publishBlock block.header
                    
                if Block.isGenesis chain block then 
                     return! handleGenesisBlock chain timestamp state blockHash block                                                                                      
                else
                    // try to find parent block                               
                    match BlockRepository.tryFind state.blockRepository block.header.parent with
                    | None ->
                        Log.info "Adding block as orphan #%d %A" block.header.blockNumber blockHash
                        
                        let persistentBlock = PersistentBlock.createOrphan blockHash block
                        do! BlockRepository.insert state.blockRepository persistentBlock
                        
                        if not <| Map.containsKey block.header.parent state.blockRequests then
                            // asking network for the parent block
                            let blockRequests = Map.add block.header.parent ParentBlock state.blockRequests
                            do! getBlock block.header.parent
                            
                            return {state with blockRequests=blockRequests}
                        else                            
                            return state
                    | Some parent ->
                        match PersistentBlock.status parent with 
                        | PersistentBlock.Invalid ->
                            // Ignoring the block
                            return state
                        | PersistentBlock.Orphan ->
                            let persistentBlock = PersistentBlock.createOrphan blockHash block
                            do! BlockRepository.insert state.blockRepository persistentBlock

                            return state
                        | PersistentBlock.Tip ->
                            return! handleMainChain chain timestamp state parent blockHash block
                        | PersistentBlock.Connected ->
                            return!  handleForkChain chain timestamp state parent blockHash block
    }

let handleNewBlockHeader chain peerId header (state:State) =
    effectsWriter {            
        let blockHash = BlockHeader.hash header
       
        if (not <| BlockRepository.exist state.blockRepository blockHash) && (not <| Map.containsKey blockHash state.blockRequests) then
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

let handleTip chain header (state:State) =
    effectsWriter {
        let blockHash = BlockHeader.hash header                       
    
        if (not <| BlockRepository.exist state.blockRepository blockHash) && (not <| Map.containsKey blockHash state.blockRequests) then
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