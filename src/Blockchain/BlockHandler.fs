module Blockchain.BlockHandler

open Blockchain
open Blockchain.ChainConnection
open Consensus
open Types
open Infrastructure
open Blockchain.EffectsWriter
open Blockchain.Tally.Handler
open Blockchain.Tally.Repository
open Messaging
open Events
open State
open Logary.Message
open Environment

module ExtHeader = ExtendedBlockHeader

type ExtHeader = ExtHeader.T

let loadContract
    ( contractPath : string     )
    ( expiry       : uint32     )
    ( code         : string     )
    ( contractId   : ContractId )
    : Contract.Contract =
    match Contract.load contractPath expiry code contractId with
    | Error error ->
        failwith error    // We should abort node if we fail to load loadable contract
    | Ok contract ->
        contract

/// Publish BlockRemoved events for a chain from tip to fork block
let private publishRemoveBlocks
    ( env       : Env       )
    ( forkBlock : ExtHeader )
    ( tip       : ExtHeader )
    : EffectsWriter<unit> =
    effectsWriter {
        let headers =
            Chain.getSubChain env.session forkBlock tip
            |> List.rev

        for header in headers do
            // Change status of the header from main to connected
            ExtHeader.unmarkAsMain header
            |> BlockRepository.saveHeader env.session

            let block =
                BlockRepository.getFullBlock env.session header

            eventX "BlockHandler: BlockRemoved #{blockNumber}"
            >> setField "blockNumber" header.header.blockNumber
            |> Log.info

            do! publish (BlockRemoved (header.hash, block))
    }

/// Publish BlockAdded events for a chain from tip to fork block
let private publishAddBlocks
    ( env       : Env       )
    ( forkBlock : ExtHeader )
    ( tip       : ExtHeader )
    : EffectsWriter<unit> =
    effectsWriter {
        let headers =
            Chain.getSubChain env.session forkBlock tip

        for header in headers do
            // Change status of the header from main to connected
            ExtHeader.markAsMain header
            |> BlockRepository.saveHeader env.session

            let block =
                BlockRepository.getFullBlock env.session header

            eventX "BlockHandler: BlockAdded #{blockNumber} hash #{blockHash}"
            >> setField "blockNumber" header.header.blockNumber
            >> setField "blockHash"   (Hash.toString header.hash)
            |> Log.info

            do! publish (BlockAdded (header.hash, block))
    }

/// Undo blocks from current state in order to get the state of the forkblock
#if DEBUG
let undoBlocks
#else
let private undoBlocks
#endif
    ( env   : Env       )
    ( state     : State     )
    ( forkBlock : ExtHeader )
    ( tip       : ExtHeader )
    : OriginState  =

    let rec undoBlocksRec (origState : OriginState) : OriginState =
        if origState.header.header = forkBlock.header then
            origState
        else
            let fullBlock =
                origState.header
                |> BlockRepository.getFullBlock env.session

            let blockState =
                origState.header.hash
                |> BlockRepository.getBlockState env.session

            let parentBlockState =
                origState.header.header.parent
                |> BlockRepository.getBlockState env.session
            undoBlocksRec
                {
                    header =
                        origState.header.header.parent
                        |> BlockRepository.getHeader env.session
                    connection =
                        {
                            utxoSet =
                                origState.utxoSet
                                |> UtxoSet.undoBlock (UtxoSetRepository.get env.session) fullBlock
                            acs =
                                origState.acs
                                |> ActiveContractSet.undoBlock (loadContract env.session.context.contractPath) blockState.activeContractSetUndoData
                            cgp =
                                parentBlockState.cgp
                            ema =
                                parentBlockState.ema
                            contractCache =
                                origState.contractCache
                            contractStates =
                                origState.contractStates
                                |> ContractStates.undoBlock blockState.contractStatesUndoData
                        }
                    mempool =
                        origState.mempool
                        |> MemPool.undoBlock fullBlock
                }

    undoBlocksRec
            {
                header =
                    tip
                connection =
                    {
                        utxoSet =
                            UtxoSet.asDatabase
                        acs =
                            state.tipState.activeContractSet
                        cgp =
                            state.cgp
                        ema =
                            (BlockRepository.getBlockState env.session tip.hash).ema
                        contractCache =
                            state.memoryState.contractCache
                        contractStates =
                            ContractStates.asDatabase
                    }
                mempool =
                    state.memoryState.mempool
            }

// After applying block or blocks we must read mempool transactions to the ACS and UTXO
let getMemoryState
    ( env           : Env                 )
    ( blockNumber   : uint32              )
    ( mempool       : MemPool.T           )
    ( orphanPool    : OrphanPool.T        )
    ( acs           : ActiveContractSet.T )
    ( contractCache : ContractCache.T     )
    : EffectsWriter< MemoryState> =

    // We start with an empty mempool and current orphan pool
    // We first validate all orphan transactions according to the new state
    // We loop through existing mempool and adding it as we go to a new mempool
    // We don't publish AddedMemPool event for mempool transaction as they are already in mempool
    // We only publish add to mem pool transactions to orphan transactions
    let memoryState = {
        utxoSet           = UtxoSet.asDatabase
        activeContractSet = acs
        mempool           = MemPool.empty
        orphanPool        = orphanPool
        contractCache     = contractCache
        contractStates    = ContractStates.asDatabase
        invalidTxHashes   = Set.empty
    }

    let memoryState =
        TransactionHandler.validateOrphanTransactions env.chainParams env.session env.contractsPath blockNumber env.timestamp memoryState

    Map.fold (fun writer _ (_,ex) ->
        Writer.bind writer (fun memoryState ->
            TransactionHandler.validateInputs env.chainParams env.session env.contractsPath blockNumber env.timestamp ex memoryState false
        )) memoryState mempool

let rollForwardChain
    ( env             : Env                 )
    ( state           : State               )
    ( block           : Block               )
    ( persistentBlock : ExtHeader           )
    ( acs             : ActiveContractSet.T )
    ( ema             : EMA.T               )
    ( cgp             : CGP.T               )
    : EffectsWriter< State>
    =
    effectsWriter {
        // unorphan any orphan chain starting with current block
        Chain.unorphanChain env.session persistentBlock

        let mempool =
            MemPool.handleBlock block state.memoryState.mempool

        let currentChainWork =
            ExtHeader.chainWork persistentBlock

        // find all longer chain and connect the longest, chains are ordered
        let chains =
            Chain.findLongerChains env.session persistentBlock currentChainWork

        eventX "BlockHandler: Connecting to longest chain"
        |> Log.info

        let origState =
            {
                header =
                    persistentBlock
                connection =
                    {
                        utxoSet        = UtxoSet.asDatabase
                        acs            = acs
                        cgp            = cgp
                        ema            = ema
                        contractCache  = state.memoryState.contractCache
                        contractStates = ContractStates.asDatabase
                    }
                mempool =
                    mempool
            }

        match ChainConnection.connectLongestChain env origState chains currentChainWork Operation.Main with
        | Some origState ->
            let tip =
                ExtHeader.markAsMain origState.header

            Chain.updateTip env.session origState.connection tip

            let acs =
                ActiveContractSet.clearChanges origState.acs

            let tipState =
                {
                    activeContractSet = acs
                    ema               = origState.ema
                    tip               = tip
                }

            let! memoryState =
                getMemoryState { env with timestamp = tip.header.timestamp } tip.header.blockNumber origState.mempool state.memoryState.orphanPool acs origState.contractCache

            do! publishAddBlocks env persistentBlock tip

            eventX "BlockHandler: New tip #{blockNumber} {tip}"
            >> setField "blockNumber" tipState.tip.header.blockNumber
            >> setField "tip" (Hash.toString tipState.tip.hash)
            |> Log.info

            // Not publishing new blocks during the initial download phase
            if not <| InitialBlockDownload.isActive state.initialBlockDownload then
                do! publishBlock tip.header

            return
                { state with
                    tipState    = tipState
                    memoryState = memoryState
                    cgp         = origState.cgp
                }
        | None ->
            let tipState =
                {
                    activeContractSet = acs
                    ema               = ema
                    tip               = persistentBlock
                }

            let! memoryState =
                getMemoryState env persistentBlock.header.blockNumber mempool state.memoryState.orphanPool acs state.memoryState.contractCache

            eventX "BlockHandler: New tip #{blockNumber} {tip}"
            >> setField "blockNumber" tipState.tip.header.blockNumber
            >> setField "tip" (Hash.toString tipState.tip.hash)
            |> Log.info

            // Not publishing new blocks during the initial download phase
            if not <| InitialBlockDownload.isActive state.initialBlockDownload then
                do! publishBlock block.header

            return
                { state with
                    tipState    = tipState
                    memoryState = memoryState
                    cgp         = cgp
                }
    }

let private handleGenesisBlock
    ( env       : Env       )
    ( state     : State     )
    ( blockHash : Hash.Hash )
    ( block     : Block     )
    : EffectsWriter<State> =
    effectsWriter {

        let connState : BlockConnection.State = {
            utxoSet        = UtxoSet.asDatabase
            acs            = ActiveContractSet.empty
            cgp            = CGP.empty
            ema            = EMA.create env.chainParams
            contractCache  = state.memoryState.contractCache
            contractStates = ContractStates.asDatabase
        }

        let connEnv : BlockConnection.Env = {
            chainParams      = env.chainParams
            timestamp        = env.timestamp
            getUTXO          = UtxoSetRepository.get env.session
            getContractState = ContractStateRepository.get env.session
            contractsPath    = env.contractsPath
            parent           = Block.genesisParent
            block            = block
        }

        match BlockConnection.connect connState connEnv with
        | Error error ->
            eventX "Failed connecting genesis block {hash} due to {error}"
            >> setField "hash" (block.header |> Block.hash |> Hash.toString)
            >> setField "error" error
            |> Log.info

            let! ibd =
                InitialBlockDownload.invalid env.timestamp blockHash state.initialBlockDownload

            return { state with initialBlockDownload = ibd }
        | Ok (block, connState) ->

            eventX "BlockHandler: Genesis block received"
            |> Log.info

            let extendedHeader =
                ExtHeader.createGenesis blockHash block

            Tally.Handler.addBlock env.session env.chainParams connState.utxoSet block

            BlockRepository.saveHeader env.session extendedHeader

            BlockRepository.saveFullBlock env.session blockHash block

            BlockRepository.saveBlockState env.session blockHash
                {
                    ema =
                        connState.ema
                    cgp =
                        connState.cgp
                    activeContractSetUndoData =
                        ActiveContractSet.getUndoData connState.acs state.tipState.activeContractSet
                    contractStatesUndoData =
                        ContractStates.getUndoData (ContractStateRepository.get env.session) connState.contractStates ContractStates.asDatabase
                }

            BlockRepository.saveGenesisHash env.session extendedHeader.hash

            Chain.updateTip env.session connState extendedHeader

            let acs =
                ActiveContractSet.clearChanges connState.acs

            do! publish (BlockAdded (blockHash, block))

            let! ibd =
                InitialBlockDownload.received env.timestamp blockHash state.initialBlockDownload

            let state =
                { state with
                    cgp =
                        connState.cgp
                    initialBlockDownload =
                        ibd
                    memoryState =
                        { state.memoryState with
                            contractStates =
                                connState.contractStates
                            contractCache =
                                connState.contractCache
                        }
                }

            return! rollForwardChain env state block extendedHeader acs connState.ema connState.cgp
    }

// Handle new block that is extending the main chain
// New block can also unorphan a chain and extend the chain even further
// So we also have to unorphan any chain and find longest chain
let private handleMainChain
    ( env       : Env       )
    ( state     : State     )
    ( parent    : ExtHeader )
    ( blockHash : Hash.Hash )
    ( block     : Block     )
    : EffectsWriter<State> =
    effectsWriter {

        let state =
            if CGP.isPayoutBlock env.chainParams (block.header.blockNumber) then
                let interval = CGP.getInterval env.chainParams block.header.blockNumber
                let winner   = Tally.Handler.getWinner env.session interval
                let cgp      = CGP.update winner state.cgp

                eventX "BlockHandler: State Changed #{blockNumber}"
                >> setField "blockNumber" block.header.blockNumber
                |> Log.info
                CGP.log cgp
                { state with cgp = cgp}
            else
                state
        let connState : BlockConnection.State = {
            utxoSet        = UtxoSet.asDatabase
            acs            = state.tipState.activeContractSet
            cgp            = state.cgp
            ema            = state.tipState.ema
            contractCache  = state.memoryState.contractCache
            contractStates = ContractStates.asDatabase
        }

        let connEnv : BlockConnection.Env = {
            chainParams      = env.chainParams
            timestamp        = env.timestamp
            getUTXO          = UtxoSetRepository.get env.session
            getContractState = ContractStateRepository.get env.session
            contractsPath    = env.contractsPath
            parent           = parent.header
            block            = block
        }

        match BlockConnection.connect connState connEnv with
        | Error error ->
            eventX "Failed connecting block #{blockNumber} {hash} due to {error}"
            >> setField "blockNumber" block.header.blockNumber
            >> setField "hash" (block.header |> Block.hash |> Hash.toString)
            >> setField "error" error
            |> Log.info

            let! ibd =
                InitialBlockDownload.invalid env.timestamp blockHash state.initialBlockDownload

            return { state with initialBlockDownload = ibd }
        | Ok (block, connState) ->
            eventX "BlockHandler: New block #{blockNumber} of {timestamp} with {txs} txs 0x{difficulty} {blockHash}"
            >> setField "blockNumber" block.header.blockNumber
            >> setField "timestamp" (Timestamp.toString env.timestamp)
            >> setField "txs" (List.length block.transactions)
            >> setField "difficulty" block.header.difficulty
            >> setField "blockHash" (Hash.toString blockHash)
            |> Log.info

            let! initialBlockDownload =
                InitialBlockDownload.received env.timestamp blockHash state.initialBlockDownload

            let state =
                { state with
                    initialBlockDownload =
                        initialBlockDownload
                    memoryState =
                        { state.memoryState with contractCache = connState.contractCache }
                }
            Tally.Handler.addBlock env.session env.chainParams connState.utxoSet block

            let extendedHeader =
                ExtHeader.createMain parent blockHash block

            BlockRepository.saveHeader env.session extendedHeader

            BlockRepository.saveFullBlock env.session blockHash block

            BlockRepository.saveBlockState env.session extendedHeader.hash
                {
                    ema =
                        connState.ema
                    cgp =
                        state.cgp
                    activeContractSetUndoData =
                        ActiveContractSet.getUndoData connState.acs state.tipState.activeContractSet
                    contractStatesUndoData =
                        ContractStates.getUndoData (ContractStateRepository.get env.session) connState.contractStates ContractStates.asDatabase
                }

            Chain.updateTip env.session connState extendedHeader

            let acs =
                ActiveContractSet.clearChanges connState.acs


            eventX "BlockHandler: BlockAdded #{blockNumber} hash #{blockHash}"
            >> setField "blockNumber" block.header.blockNumber
            >> setField "blockHash"   (Hash.toString blockHash)
            |> Log.info

            // Pulishing event of the new block
            do! publish (BlockAdded (blockHash, block))

            return! rollForwardChain env state block extendedHeader acs connState.ema connState.cgp
    }

/// New block that is extending one of the fork chains
/// We first unorphan any chain and look for any chain that is longer than main chain
/// We then try to connect any of the longer chain, and check that the connected chain is actually longer
/// Finally we raise events that remove blocks from main chain up to the fork block and raise add events for all new blocks
let private handleForkChain
    ( env       : Env       )
    ( state     : State     )
    ( parent    : ExtHeader )
    ( blockHash : Hash.Hash )
    ( block     : Block     )
    : EffectsWriter< State> =
    effectsWriter {
        let extendedHeader =
            ExtHeader.createConnected parent blockHash block

        BlockRepository.saveHeader env.session extendedHeader

        BlockRepository.saveFullBlock env.session blockHash block

        // unorphan any orphan chain starting with current block
        Chain.unorphanChain env.session extendedHeader

        let currentTip =
            state.tipState.tip

        let currentChainWork =
            ExtHeader.chainWork currentTip

        let! ibd =
            InitialBlockDownload.received env.timestamp blockHash state.initialBlockDownload

        let state =
            { state with initialBlockDownload = ibd }

        match Chain.findLongerChains env.session extendedHeader currentChainWork with
        | [] ->
            // No chain is longer than current chain

            eventX "Block added to a fork chain #{blockNumber} {hash}"
            >> setField "blockNumber" block.header.blockNumber
            >> setField "hash" (Hash.toString blockHash)
            |> Log.info

            return state
        | chains ->
            let forkBlock =
                Chain.findForkBlock env.session extendedHeader currentTip

            let forkState =
                undoBlocks env state forkBlock currentTip
            match ChainConnection.connectLongestChain env forkState chains currentChainWork Operation.Fork with
            | None ->
                eventX "Block added to a fork chain #{blockNumber} {hash}"
                >> setField "blockNumber" block.header.blockNumber
                >> setField "hash" (Hash.toString blockHash)
                |> Log.info

                // No connectable chain is not longer than current chain
                return state
            | Some origState ->
                eventX "Reorg to #{blockNumber} {hash}"
                >> setField "blockNumber" origState.header.header.blockNumber
                >> setField "hash" (Hash.toString origState.header.hash)
                |> Log.info

                let tip =
                    ExtHeader.markAsMain origState.header

                removeTallyBlocks env state.memoryState.utxoSet forkBlock currentTip
                addTallyBlocks env forkState.utxoSet forkBlock tip

                Chain.updateTip env.session origState.connection tip

                let acs =
                    ActiveContractSet.clearChanges origState.acs

                do! publishRemoveBlocks env forkBlock currentTip

                do! publishAddBlocks env forkBlock tip

                let tipState =
                    {
                        activeContractSet = acs
                        ema               = origState.ema
                        tip               = tip
                    }

                let! memoryState =
                    getMemoryState env tip.header.blockNumber origState.mempool state.memoryState.orphanPool acs origState.contractCache

                // Not publishing new blocks during the initial download phase
                if not <| InitialBlockDownload.isActive state.initialBlockDownload then
                    do! publishBlock tip.header

                return
                    { state with
                        tipState    = tipState
                        memoryState = memoryState
                        cgp         = origState.cgp
                    }
    }

let private handleHeader
    ( env    : Env                              )
    ( get    : Hash.Hash -> EffectsWriter<unit> )
    ( header : BlockHeader                      )
    ( state  : State                            )
    : EffectsWriter<State> =
    effectsWriter {
        let blockHash =
            Block.hash header

        match BlockRepository.tryGetHeader env.session blockHash with
        | Some extendedBlock ->
            if extendedBlock.status = ExtHeader.Orphan then
                let headers =
                    if state.headers < header.blockNumber then
                        header.blockNumber
                    else
                        state.headers

                let state =
                    { state with headers = headers }

                return! Chain.requestOrphanChainRoot env.session get extendedBlock state
            else
                return state
        | None ->
            match BlockValidation.Header.validate env.chainParams header with
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

                return { state with headers = headers }
            | Error _ ->
                return state
    }

let validateNewBlockHeader
    ( env    : Env         )
    ( peerId : byte[]      )
    ( header : BlockHeader )
    ( state  : State       )
    : EffectsWriter<State> =
    effectsWriter {
        // we ignore new blocks while IBD is in progress
        if InitialBlockDownload.isActive state.initialBlockDownload then

            return state

        else

            eventX "Handling new block header #{blockNumber}"
            >> setField "blockNumber" header.blockNumber
            |> Log.info

            return! handleHeader env (getBlockFrom peerId) header state
    }

let handleTip
    ( env    : Env         )
    ( peerId : byte[]      )
    ( header : BlockHeader )
    ( state  : State       )
    : EffectsWriter<State> =
    effectsWriter {
        if InitialBlockDownload.isActive state.initialBlockDownload then

            return state

        elif InitialBlockDownload.shouldStartInitialBlockDownload state.tipState.tip.header header then

            let! initialBlockDownload =
                InitialBlockDownload.start env.timestamp state.tipState.tip.hash state.tipState.tip.header peerId header

            return { state with initialBlockDownload = initialBlockDownload }

        else

            return! handleHeader env (getBlockFrom peerId) header state
    }

let validateBlock
    ( env    : Env           )
    ( peerId : byte[] option )
    ( block  : Block         )
    ( state  : State         )
    : EffectsWriter<State> =

    effectsWriter {
        let blockHash =
            Block.hash block.header
        // checking if block already exist
        if BlockRepository.contains env.session blockHash then return state else
        eventX "Validating new block #{blockNumber} with {txs} txs {hash}"
        >> setField "blockNumber" block.header.blockNumber
        >> setField "txs" (List.length block.transactions)
        >> setField "hash" (Hash.toString blockHash)
        |> Log.info
        match BlockValidation.validate block env.chainParams with
        | Error error ->
            eventX "Block {hash} failed validation due to {error}"
            >> setField "hash" (Hash.toString blockHash)
            >> setField "error" error
            |> Log.info

            let! initialBlockDownload = InitialBlockDownload.invalid env.timestamp blockHash state.initialBlockDownload

            return {state with initialBlockDownload = initialBlockDownload}
        | Ok block ->
            if Block.isGenesis env.chainParams block then
                let! state' = handleGenesisBlock env state blockHash block

                if state'.tipState.tip <> state.tipState.tip then
                    do! publish (TipChanged state'.tipState.tip.header)

                return state'
            else
                // try to find parent block
                match BlockRepository.tryGetHeader env.session block.header.parent with
                | None ->
                    eventX "Adding block as orphan #{blockNumber} {blockHash}"
                    >> setField "blockNumber" block.header.blockNumber
                    >> setField "blockHash" (Hash.toString blockHash)
                    |> Log.info

                    let extendedHeader = ExtHeader.createOrphan blockHash block
                    BlockRepository.saveHeader env.session extendedHeader
                    BlockRepository.saveFullBlock env.session blockHash block

                    let! initialBlockDownload = InitialBlockDownload.received env.timestamp blockHash state.initialBlockDownload

                    let state = { state with initialBlockDownload = initialBlockDownload }

                    match peerId with
                    | Some peerId ->
                        do! getBlockFrom peerId block.header.parent
                    | _ ->
                        do! getBlock block.header.parent

                    return state
                | Some parent ->
                    match ExtHeader.status parent with
                    | ExtHeader.Invalid ->
                        // Ignoring the block

                        let! initialBlockDownload = InitialBlockDownload.invalid env.timestamp blockHash state.initialBlockDownload

                        return {state with initialBlockDownload = initialBlockDownload}

                    | ExtHeader.Orphan ->
                        let extendedHeader = ExtHeader.createOrphan blockHash block
                        BlockRepository.saveHeader env.session extendedHeader
                        BlockRepository.saveFullBlock env.session blockHash block

                        eventX "Adding block to orphan chain #{blockNumber} {blockHash}"
                        >> setField "blockNumber" block.header.blockNumber
                        >> setField "blockHash" (Hash.toString blockHash)
                        |> Log.info

                        let! initialBlockDownload = InitialBlockDownload.received env.timestamp blockHash state.initialBlockDownload

                        let state = { state with initialBlockDownload = initialBlockDownload }

                        match peerId with
                        | Some peerId ->
                            return! Chain.requestOrphanChainRoot env.session (getBlockFrom peerId) parent state
                        | _ ->
                            return! Chain.requestOrphanChainRoot env.session getBlock parent state
                    | ExtHeader.MainChain
                    | ExtHeader.Connected ->

                        let! state' =
                            if parent.hash = state.tipState.tip.hash then
                                handleMainChain env state parent blockHash block
                            else
                                handleForkChain env state parent blockHash block

                        if state'.tipState.tip <> state.tipState.tip then
                            do! publish (TipChanged state'.tipState.tip.header)

                        return state'
    }
