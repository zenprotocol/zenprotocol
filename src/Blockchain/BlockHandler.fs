module Blockchain.BlockHandler

open Blockchain
open Consensus
open Types
open Infrastructure
open Blockchain.EffectsWriter
open Consensus
open Consensus.Chain
open Messaging.Events
open State
open Logary.Message

let getUTXO = UtxoSetRepository.get
let getTx = TransactionRepository.tryGetTransaction
let getContractState = ContractStateRepository.get
let loadContract contractPath expiry code contractId =
    match Contract.load contractPath expiry code contractId with
    | Error error-> failwith error // We should abort node if we fail to load loadable contract
    | Ok contract -> contract

// Change the status of the entire chain from the root orphan block up to all tips from Orphan to Connected
let private unorphanChain session (root:ExtendedBlockHeader.T) =
    let unorphanBlock (block:ExtendedBlockHeader.T) =
        if root.header <> block.header then
            let parent = BlockRepository.getHeader session block.header.parent
            let parentChainWork = ExtendedBlockHeader.chainWork parent
            let chainWork = Block.getChainWork parentChainWork block.header

            ExtendedBlockHeader.unorphan block chainWork
            |> BlockRepository.saveHeader session

    Tree.iter session unorphanBlock root

// find chains that are longer than the minChainWork
// this is not final as we also need to check if the chain is connectable
let private findLongerChains session extendedHeader minChainWork =
    let rec getContinuation continuation headers f =
        if Seq.isEmpty headers then
            continuation
        else
            let head = Seq.head headers
            let tail = Seq.tail headers

            fun acc -> f head acc (getContinuation continuation tail f)

    let rec findLongerChains' extendedHeader acc continuation =
        let children = BlockRepository.getBlockChildren session extendedHeader

        if Seq.isEmpty children then
            if ExtendedBlockHeader.chainWork extendedHeader > minChainWork then
                continuation (extendedHeader :: acc)
            else continuation acc
        else
            let head = Seq.head children
            let tail = Seq.tail children

            findLongerChains' head acc (getContinuation continuation tail findLongerChains')

    findLongerChains' extendedHeader [] id
    |> List.sortByDescending ExtendedBlockHeader.chainWork

// Connect the entire chain, returning the valid tip along with state
let private connectChain chainParams contractPath timestamp session (origin:ExtendedBlockHeader.T) originState (tip:ExtendedBlockHeader.T) getContractState =
    let rec getHeaders (header:ExtendedBlockHeader.T) headers =
        if header.header = origin.header then
            headers
        else
            let parent = BlockRepository.getHeader session header.header.parent

            getHeaders parent (header :: headers)

    let headers = getHeaders tip []

    List.fold (fun ((validTip:ExtendedBlockHeader.T),(utxoSet, (cgp:CGP.T), acs, ema, mempool, contractCache, contractStates)) tip ->
        if not (ExtendedBlockHeader.isValid tip) then
            validTip,(utxoSet,cgp,acs,ema,mempool,contractCache,contractStates)
        elif validTip.hash <> tip.header.parent then
            // parent block is invalid, so are we
            BlockRepository.saveHeader session (ExtendedBlockHeader.invalid tip)

            validTip,(utxoSet,cgp,acs,ema,mempool,contractCache,contractStates)
        else
            let block = BlockRepository.getFullBlock session tip

            match Block.connect chainParams (getUTXO session) (getTx session) contractPath validTip.header timestamp utxoSet cgp acs contractCache ema (getContractState session) contractStates block with
            | Error error ->
                BlockRepository.saveHeader session (ExtendedBlockHeader.invalid tip)

                eventX "Failed connecting block #{blockNumber} {hash} due to {error}"
                >> setField "blockNumber" block.header.blockNumber
                >> setField "hash" (Hash.toString tip.hash)
                >> setField "error" error
                |> Log.info

                validTip,(utxoSet,cgp,acs,ema,mempool,contractCache,contractStates)
            | Ok (block,utxoSet,cgp,acs',contractCache,ema,contractStates') ->
                let acsUndoData = ActiveContractSet.getUndoData acs' acs
                let contractStatesUndoData = ContractStates.getUndoData (getContractState session) contractStates' contractStates

                BlockRepository.saveBlockState session tip.hash acsUndoData contractStatesUndoData ema cgp

                let mempool = MemPool.handleBlock block mempool

                tip,(utxoSet,cgp,acs',ema,mempool,contractCache,contractStates')) (origin, originState) headers

let private connectLongestChain chainParams contractPath timestamp session origin originState chains minChainWork =
    let connectChain tip (best,state) =
        // We are checking twice if current is longer than the bestChain, once before connecting and once after
        if ExtendedBlockHeader.chainWork tip > ExtendedBlockHeader.chainWork best then
            let chain,state = connectChain chainParams contractPath timestamp session origin originState tip getContractState

            if ExtendedBlockHeader.chainWork chain > ExtendedBlockHeader.chainWork best then
                chain,state
            else
                best,state
        else
            best,state

    let best,bestState =
        List.fold (fun (best,state) chain -> connectChain chain (best,state)) (origin,originState) chains

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

let private getSubChain session (start:ExtendedBlockHeader.T) (tip:ExtendedBlockHeader.T) =
    let rec getSubChain' (current:ExtendedBlockHeader.T) acc =
        if current.header = start.header then
            acc
        else
            let parent = BlockRepository.getHeader session current.header.parent

            getSubChain' parent (current :: acc)

    getSubChain' tip []

// Publish BlockRemoved events for a chain from tip to fork block
let private removeBlocks session (forkBlock:ExtendedBlockHeader.T) (tip:ExtendedBlockHeader.T) =
    effectsWriter {
        let blocks =
            getSubChain session forkBlock tip
            |> List.rev

        for block in blocks do
            // Change status of the header from main to connected
            ExtendedBlockHeader.unmarkAsMain block
            |> BlockRepository.saveHeader session

            let fullBlock = BlockRepository.getFullBlock session block
            do! publish (BlockRemoved (block.hash, fullBlock))

        return ()
    }

// Publish BlockAdded events for a chain from tip to fork block
let rec private addBlocks session (forkBlock:ExtendedBlockHeader.T) (tip:ExtendedBlockHeader.T) =
    effectsWriter {
        let blocks = getSubChain session forkBlock tip

        for block in blocks do
            // Change status of the header from main to connected
            ExtendedBlockHeader.markAsMain block
            |> BlockRepository.saveHeader session

            let fullBlock = BlockRepository.getFullBlock session block

            eventX "BlockHandler: BlockAdded #{blockNumber}"
            >> setField "blockNumber" block.header.blockNumber
            |> Log.info

            do! publish (BlockAdded (block.hash, fullBlock))

        return ()
    }

// Undo blocks from current state in order to get the state of the forkblock
let rec private undoBlocks (chain:ChainParameters) session (forkBlock:ExtendedBlockHeader.T) (tip:ExtendedBlockHeader.T) utxoSet (cgp:CGP.T) acs contractStates mempool contractCache  =
    let blockState = BlockRepository.getBlockState session tip.hash

    if tip.header = forkBlock.header then
        (utxoSet,cgp,acs,blockState.ema,mempool,contractCache,contractStates)
    else
        let parent = BlockRepository.getHeader session tip.header.parent
        let fullBlock = BlockRepository.getFullBlock session tip

        let mempool = MemPool.undoBlock fullBlock mempool
        let utxoSet =
            UtxoSet.undoBlock                
                (getUTXO session)
                fullBlock utxoSet
                
        let parntBlockState = BlockRepository.getBlockState session tip.header.parent
                
        let acs = ActiveContractSet.undoBlock (loadContract session.context.contractPath) blockState.activeContractSetUndoData acs
        let contractStates = ContractStates.undoBlock blockState.contractStatesUndoData contractStates
        undoBlocks chain session forkBlock parent utxoSet parntBlockState.cgp acs contractStates mempool contractCache

// After applying block or blocks we must read mempool transactions to the ACS and UTXO
let getMemoryState chainParams session contractPath blockNumber timestamp mempool orphanPool acs contractCache =

    // We start with an empty mempool and current orphan pool
    // We first validate all orphan transactions according to the new state
    // We loop through existing mempool and adding it as we go to a new mempool
    // We don't publish AddedMemPool event for mempool transaction as they are already in mempool
    // We only publish add to mem pool transactions to orphan transactions
    let memoryState = {
        utxoSet = UtxoSet.asDatabase
        activeContractSet = acs
        mempool = MemPool.empty
        orphanPool = orphanPool
        contractCache = contractCache
        contractStates = ContractStates.asDatabase
        invalidTxHashes = Set.empty
    }


    let memoryState = TransactionHandler.validateOrphanTransactions chainParams session contractPath blockNumber timestamp memoryState

    Map.fold (fun writer _ (_,ex) ->
        Writer.bind writer (fun memoryState ->
            TransactionHandler.validateInputs chainParams session contractPath blockNumber timestamp ex memoryState false
        )) memoryState mempool


let private rollForwardChain chainParams contractPath timestamp state session block persistentBlock acs ema cgp =
    effectsWriter {
        // unorphan any orphan chain starting with current block
        unorphanChain session persistentBlock

        let mempool = MemPool.handleBlock block state.memoryState.mempool

        // find all longer chain and connect the longest, chains are ordered
        let currentChainWork = ExtendedBlockHeader.chainWork persistentBlock
        let chains = findLongerChains session persistentBlock currentChainWork

        eventX "BlockHandler: Connecting to longest chain"
        |> Log.info

        let chain' =
            connectLongestChain chainParams contractPath timestamp session persistentBlock
                (UtxoSet.asDatabase,cgp,acs,ema,mempool,state.memoryState.contractCache,ContractStates.asDatabase) chains currentChainWork
        match chain' with
        | Some (tip,(utxoSet,cgp,acs,ema,mempool,contractCache,contractStates)) ->

            let tip = ExtendedBlockHeader.markAsMain tip

            // update tip
            BlockRepository.updateTip session tip.hash

            UtxoSetRepository.save session utxoSet
            ContractStateRepository.save session contractStates
            ActiveContractSetRepository.save session acs
            let acs = ActiveContractSet.clearChanges acs

            let tipState = {activeContractSet=acs;ema=ema;tip=tip;cgp=cgp}
            let! memoryState = getMemoryState chainParams session contractPath tip.header.blockNumber tip.header.timestamp mempool state.memoryState.orphanPool acs contractCache

            do! addBlocks session persistentBlock tip

            eventX "BlockHandler: New tip #{blockNumber} {tip}"
            >> setField "blockNumber" tipState.tip.header.blockNumber
            >> setField "tip" (Hash.toString tipState.tip.hash)
            |> Log.info

            // Not publishing new blocks during the initial download phase
            if not <| InitialBlockDownload.isActive state.initialBlockDownload then
                do! publishBlock tip.header

            return {state with tipState=tipState;memoryState=memoryState}
        | None ->
            let tipState = {activeContractSet=acs;ema=ema;tip=persistentBlock;cgp=cgp}
            let! memoryState = getMemoryState chainParams session contractPath persistentBlock.header.blockNumber timestamp mempool state.memoryState.orphanPool acs state.memoryState.contractCache

            eventX "BlockHandler: New tip #{blockNumber} {tip}"
            >> setField "blockNumber" tipState.tip.header.blockNumber
            >> setField "tip" (Hash.toString tipState.tip.hash)
            |> Log.info

            // Not publishing new blocks during the initial download phase
            if not <| InitialBlockDownload.isActive state.initialBlockDownload then
                do! publishBlock block.header

            return {state with tipState=tipState;memoryState=memoryState}
    }

let private handleGenesisBlock chainParams contractPath session timestamp (state:State) blockHash block =
    effectsWriter {
        match Block.connect chainParams (getUTXO session) (getTx session) contractPath Block.genesisParent timestamp UtxoSet.asDatabase CGP.empty
                ActiveContractSet.empty state.memoryState.contractCache (EMA.create chainParams) (getContractState session) ContractStates.asDatabase block with
        | Error error ->
            eventX "Failed connecting genesis block {hash} due to {error}"
            >> setField "hash" (block.header |> Block.hash |> Hash.toString)
            >> setField "error" error
            |> Log.info

            let! ibd = InitialBlockDownload.invalid timestamp blockHash state.initialBlockDownload

            return {state with initialBlockDownload = ibd}
        | Ok (block,utxoSet,cgp,acs,contractCache,ema,contractStates) ->

            eventX "BlockHandler: Genesis block received"
            |> Log.info

            let extendedHeader = ExtendedBlockHeader.createGenesis blockHash block
            BlockRepository.saveHeader session extendedHeader
            BlockRepository.saveFullBlock session blockHash block

            let acsUndoData = ActiveContractSet.getUndoData acs state.tipState.activeContractSet
            let contractStatesUndoData = ContractStates.getUndoData (getContractState session) contractStates ContractStates.asDatabase

            BlockRepository.saveBlockState session blockHash acsUndoData contractStatesUndoData ema cgp

            BlockRepository.updateTip session extendedHeader.hash
            BlockRepository.saveGenesisHash session extendedHeader.hash

            UtxoSetRepository.save session utxoSet
            ContractStateRepository.save session contractStates
            ActiveContractSetRepository.save session acs
            let acs = ActiveContractSet.clearChanges acs

            do! publish (BlockAdded (blockHash, block))

            let! ibd = InitialBlockDownload.received timestamp blockHash state.initialBlockDownload

            let state = {state with initialBlockDownload = ibd; memoryState = { state.memoryState with contractStates = contractStates; contractCache = contractCache }}

            return! rollForwardChain chainParams contractPath timestamp state session block extendedHeader acs ema cgp
    }

// Handle new block that is extending the main chain
// New block can also unorphan a chain and extend the chain even further
// So we also have to unorphan any chain and find longest chain
let private handleMainChain chain contractPath session timestamp (state:State) (parent:ExtendedBlockHeader.T) blockHash block =
    effectsWriter {
        match Block.connect chain (getUTXO session) (getTx session) contractPath parent.header timestamp UtxoSet.asDatabase state.tipState.cgp
                state.tipState.activeContractSet state.memoryState.contractCache state.tipState.ema (getContractState session) ContractStates.asDatabase block with
        | Error error ->
            eventX "Failed connecting block #{blockNumber} {hash} due to {error}"
            >> setField "blockNumber" block.header.blockNumber
            >> setField "hash" (block.header |> Block.hash |> Hash.toString)
            >> setField "error" error
            |> Log.info

            let! initialBlockDownload = InitialBlockDownload.invalid timestamp blockHash state.initialBlockDownload
            let state = { state with initialBlockDownload = initialBlockDownload }

            return state
        | Ok (block,utxoSet,cgp,acs,contractCache,ema,contractStates) ->
            eventX "BlockHandler: New block #{blockNumber} {timestamp} with {txs} txs 0x{difficulty} {blockHash}"
            >> setField "blockNumber" block.header.blockNumber
            >> setField "timestamp" (Timestamp.toString timestamp)
            >> setField "txs" (List.length block.transactions)
            >> setField "difficulty" block.header.difficulty
            >> setField "blockHash" (Hash.toString blockHash)
            |> Log.info

            let! initialBlockDownload = InitialBlockDownload.received timestamp blockHash state.initialBlockDownload
            let state = { state with initialBlockDownload = initialBlockDownload;
                                     memoryState = { state.memoryState with contractCache = contractCache }}

            let extendedHeader = ExtendedBlockHeader.createMain parent blockHash block

            BlockRepository.saveHeader session extendedHeader
            BlockRepository.saveFullBlock session blockHash block

            let acsUndoData = ActiveContractSet.getUndoData acs state.tipState.activeContractSet
            let contractStatesUndoData = ContractStates.getUndoData (getContractState session) contractStates ContractStates.asDatabase

            BlockRepository.saveBlockState session extendedHeader.hash acsUndoData contractStatesUndoData ema cgp

            BlockRepository.updateTip session extendedHeader.hash

            UtxoSetRepository.save session utxoSet
            ContractStateRepository.save session contractStates
            ActiveContractSetRepository.save session acs
            let acs = ActiveContractSet.clearChanges acs

            eventX "BlockHandler: BlockAdded #{blockNumber}"
            >> setField "blockNumber" block.header.blockNumber
            |> Log.info


            // Pulishing event of the new block
            do! publish (BlockAdded (blockHash, block))

            return! rollForwardChain chain contractPath timestamp state session block extendedHeader acs ema cgp
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

        let! initialBlockDownload = InitialBlockDownload.received timestamp blockHash state.initialBlockDownload
        let state = { state with initialBlockDownload = initialBlockDownload }

        match findLongerChains session extendedHeader currentChainWork with
        | [] ->
            // No chain is longer than current chain

            eventX "Block added to a fork chain #{blockNumber} {hash}"
            >> setField "blockNumber" block.header.blockNumber
            >> setField "hash" (Hash.toString blockHash)
            |> Log.info

            return state
        | chains ->
            let forkBlock = findForkBlock session extendedHeader currentTip

            // undo blocks from mainnet to get fork block state
            let forkState = undoBlocks chain session forkBlock currentTip
                                UtxoSet.asDatabase state.tipState.cgp state.tipState.activeContractSet ContractStates.asDatabase state.memoryState.mempool state.memoryState.contractCache

            match connectLongestChain chain contractPath timestamp session forkBlock forkState chains currentChainWork with
            | None ->
                eventX "Block added to a fork chain #{blockNumber} {hash}"
                >> setField "blockNumber" block.header.blockNumber
                >> setField "hash" (Hash.toString blockHash)
                |> Log.info

                // No connectable chain is not longer than current chain
                return state
            | Some (tip,(utxoSet,cgp,acs,ema,mempool,contractCache,contractStates)) ->
                eventX "Reorg to #{blockNumber} {hash}"
                >> setField "blockNumber" tip.header.blockNumber
                >> setField "hash" (Hash.toString tip.hash)
                |> Log.info

                let tip = ExtendedBlockHeader.markAsMain tip

                // update tip
                BlockRepository.updateTip session tip.hash
                UtxoSetRepository.save session utxoSet
                ContractStateRepository.save session contractStates
                ActiveContractSetRepository.save session acs
                let acs = ActiveContractSet.clearChanges acs

                do! removeBlocks session forkBlock currentTip
                do! addBlocks session forkBlock tip

                let tipState = {activeContractSet=acs;ema=ema;tip=tip;cgp=cgp}

                let! memoryState = getMemoryState chain session contractPath tip.header.blockNumber timestamp mempool state.memoryState.orphanPool acs contractCache

                // Not publishing new blocks during the initial download phase
                if not <| InitialBlockDownload.isActive state.initialBlockDownload then
                    do! publishBlock tip.header

                return {state with tipState=tipState;memoryState=memoryState}
    }

// find orphan chain root
let requestOrphanChainRoot session timestamp get (tip:ExtendedBlockHeader.T) (state:State) =
    let rec findRoot (header:ExtendedBlockHeader.T) =
        match BlockRepository.tryGetHeader session header.header.parent with
        | Some parent -> findRoot parent
        | None -> header

    let root = findRoot tip

    effectsWriter {
        eventX "Request root of orphan chain from network block {blockNumber} {parent}"
        >> setField "blockNumber" (root.header.blockNumber - 1ul)
        >> setField "parent" (Hash.toString root.header.parent)
        |> Log.info

        do! get root.header.parent

        return state
    }

let validateBlock chainParams contractPath session timestamp peerId block mined (state:State) =
    effectsWriter {
        let blockHash = Block.hash block.header

        // checking if block already exist
        if BlockRepository.contains session blockHash then return state else

        eventX "Validating new block #{blockNumber} with {txs} txs {hash}"
        >> setField "blockNumber" block.header.blockNumber
        >> setField "txs" (List.length block.transactions)
        >> setField "hash" (Hash.toString blockHash)
        |> Log.info

        match Block.validate chainParams block with
        | Error error ->
            eventX "Block {hash} failed validation due to {error}"
            >> setField "hash" (Hash.toString blockHash)
            >> setField "error" error
            |> Log.info

            let! initialBlockDownload = InitialBlockDownload.invalid timestamp blockHash state.initialBlockDownload

            return {state with initialBlockDownload = initialBlockDownload}
        | Ok block ->
            if Block.isGenesis chainParams block then
                let! state' = handleGenesisBlock chainParams contractPath session timestamp state blockHash block

                if state'.tipState.tip <> state.tipState.tip then
                    do! publish (TipChanged state'.tipState.tip.header)

                return state'
            else
                // try to find parent block
                match BlockRepository.tryGetHeader session block.header.parent with
                | None ->
                    eventX "Adding block as orphan #{blockNumber} {blockHash}"
                    >> setField "blockNumber" block.header.blockNumber
                    >> setField "blockHash" (Hash.toString blockHash)
                    |> Log.info

                    let extendedHeader = ExtendedBlockHeader.createOrphan blockHash block
                    BlockRepository.saveHeader session extendedHeader
                    BlockRepository.saveFullBlock session blockHash block

                    let! initialBlockDownload = InitialBlockDownload.received timestamp blockHash state.initialBlockDownload

                    let state = { state with initialBlockDownload = initialBlockDownload }

                    match peerId with
                    | Some peerId ->
                        do! getBlockFrom peerId block.header.parent
                    | _ ->
                        do! getBlock block.header.parent

                    return state
                | Some parent ->
                    match ExtendedBlockHeader.status parent with
                    | ExtendedBlockHeader.Invalid ->
                        // Ignoring the block

                        let! initialBlockDownload = InitialBlockDownload.invalid timestamp blockHash state.initialBlockDownload

                        return {state with initialBlockDownload = initialBlockDownload}

                    | ExtendedBlockHeader.Orphan ->
                        let extendedHeader = ExtendedBlockHeader.createOrphan blockHash block
                        BlockRepository.saveHeader session extendedHeader
                        BlockRepository.saveFullBlock session blockHash block

                        eventX "Adding block to orphan chain #{blockNumber} {blockHash}"
                        >> setField "blockNumber" block.header.blockNumber
                        >> setField "blockHash" (Hash.toString blockHash)
                        |> Log.info

                        let! initialBlockDownload = InitialBlockDownload.received timestamp blockHash state.initialBlockDownload

                        let state = { state with initialBlockDownload = initialBlockDownload }

                        match peerId with
                        | Some peerId ->
                            return! requestOrphanChainRoot session timestamp (getBlockFrom peerId) parent state
                        | _ ->
                            return! requestOrphanChainRoot session timestamp getBlock parent state
                    | ExtendedBlockHeader.MainChain
                    | ExtendedBlockHeader.Connected ->
                        let! state' =
                            if parent.hash = state.tipState.tip.hash then
                                handleMainChain chainParams contractPath session timestamp state parent blockHash block
                            else
                                handleForkChain chainParams contractPath session timestamp state parent blockHash block

                        if state'.tipState.tip <> state.tipState.tip then
                            do! publish (TipChanged state'.tipState.tip.header)

                        return state'
    }

let private handleHeader chain session timestamp get header state =
    effectsWriter {
        let blockHash = Block.hash header

        match BlockRepository.tryGetHeader session blockHash with
        | Some extendedBlock ->
            if extendedBlock.status = ExtendedBlockHeader.Orphan then
                let headers =
                    if state.headers < header.blockNumber then
                        header.blockNumber
                    else
                        state.headers

                let state = {state with headers = headers}

                return! requestOrphanChainRoot session timestamp get extendedBlock state
            else
                return state
        | None ->
            match Block.validateHeader chain header with
            | Ok header ->
                eventX "Request block #{blockNumber} {blockHash} from peers"
                >> setField "blockNumber" header.blockNumber
                >> setField "blockHash" (Hash.toString blockHash)
                |> Log.info


                do! get blockHash

                let headers =
                    if state.headers < header.blockNumber then
                        header.blockNumber
                    else
                        state.headers

                return {state with headers=headers}
            | Error _ ->
                return state
    }

let handleNewBlockHeader chain session timestamp peerId (header:BlockHeader) (state:State) =
    effectsWriter {
        // we ignore new blocks while IBD is in progress
        if InitialBlockDownload.isActive state.initialBlockDownload then
            return state
        else
            eventX "Handling new block header #{blockNumber}"
            >> setField "blockNumber" header.blockNumber
            |> Log.info
            return! handleHeader chain session timestamp (getBlockFrom peerId) header state
    }

let handleTip chain session timestamp peerId (header:BlockHeader) (state:State) =
    effectsWriter {
        if InitialBlockDownload.isActive state.initialBlockDownload then
            return state
        elif InitialBlockDownload.shouldStartInitialBlockDownload state.tipState.tip.header header then
            let! initialBlockDownload = InitialBlockDownload.start timestamp state.tipState.tip.hash state.tipState.tip.header peerId header
            return {state with initialBlockDownload = initialBlockDownload;}
        else          
            return! handleHeader chain session timestamp (getBlockFrom peerId) header state
    }
