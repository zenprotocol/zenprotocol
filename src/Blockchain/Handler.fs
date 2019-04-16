module Blockchain.Handler

open Infrastructure
open Result
open Writer
open ServiceBus.Agent
open Messaging.Services
open Messaging.Events
open Consensus
open Blockchain
open EffectsWriter
open Consensus.Types
open State
open DatabaseContext
open Logary.Message
open Infrastructure.Result
open Chain

let getUnionCaseName (x:'a) =
    match Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

let logStartAction actionType action =
    eventX (sprintf "Blockchain: Handling %s {action} started" actionType)
    >> setField "action" (getUnionCaseName action)
    |> Log.info

let logEndAction timestamp actionType action =
    if Timestamp.now() > timestamp + 10000UL then
        eventX (sprintf "Blockchain: Handling %s {action} ended, action took more than 10 seconds" actionType)
        >> setField "action" (getUnionCaseName action)
        |> Log.warning
    else
        eventX (sprintf "Blockchain: Handling %s {action} ended" actionType)
        >> setField "action" (getUnionCaseName action)
        |> Log.info


let handleCommand chainParams command session timestamp (state:State) =
    logStartAction "command" command

    let contractPath = session.context.contractPath

    let result =
        match command with
        | ValidateTransaction ex ->
            effectsWriter {
                let! memoryState =
                    TransactionHandler.validateTransaction chainParams session contractPath state.tipState.tip.header.blockNumber
                        timestamp ex state.memoryState
                return {state with memoryState = memoryState}
            }
        | RequestMemPool peerId ->
            effectsWriter {
                let txHashes = MemPool.getTxHashes state.memoryState.mempool
                do! sendMemPool peerId txHashes
                return state
            }
        | RequestTransactions (peerId, txHashes) ->
             effectsWriter {
                let txs =
                    List.choose (fun txHash -> MemPool.getTransaction txHash state.memoryState.mempool) txHashes
                    |> List.map (fun ex-> ex.raw)

                if not <| List.isEmpty txs then
                    do! sendTransactions peerId txs

                return state
             }
        | HandleMemPool (peerId,txHashes)
        | HandleNewTransactions (peerId, txHashes) ->
            effectsWriter {
                if not <| List.exists (fun txHash -> Set.contains txHash state.memoryState.invalidTxHashes) txHashes then
                    let missingTxHashes = List.filter (fun txHash -> not <| MemPool.containsTransaction txHash state.memoryState.mempool) txHashes

                    if not <| List.isEmpty missingTxHashes then
                        do! getTransactions peerId missingTxHashes

                return state
            }
        | ValidateBlock (peerId,block) ->
            BlockHandler.validateBlock chainParams contractPath session timestamp (Some peerId) block false state
        | ValidateMinedBlock block ->
            BlockHandler.validateBlock chainParams contractPath session timestamp None block true state
        | HandleTip (peerId,header) ->
            BlockHandler.handleTip chainParams session timestamp peerId header state
        | ValidateNewBlockHeader (peerId, header) ->
            BlockHandler.handleNewBlockHeader chainParams session timestamp peerId header state
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
        | RequestHeaders (peerId, from, endHash) ->
            effectsWriter {
                if state.tipState.tip.hash <> Hash.zero then
                    do! InitialBlockDownload.getHeaders chainParams session peerId from endHash

                return state
            }
        | HandleHeaders (peerId,headers) ->
           effectsWriter {
               let! initialBlockDownload = InitialBlockDownload.processHeaders chainParams session timestamp peerId headers state.initialBlockDownload
               return {state with initialBlockDownload = initialBlockDownload}
           }

    logEndAction timestamp "command" command

    result

let handleRequest chain (requestId:RequestId) request session timestamp state =
    logStartAction "request" request

    match request with
    | ExecuteContract (contractId, command, sender, messageBody, txSkeleton) ->
        TransactionHandler.executeContract session txSkeleton timestamp contractId command sender messageBody state false
        |> requestId.reply<Result<Transaction, string>>
    | GetBlockTemplate pkHash ->
        BlockTemplateBuilder.makeTransactionList chain session state timestamp
        <@> fun (memState, validatedTransactions) ->
            Block.createTemplate chain state.tipState.tip.header timestamp state.tipState.ema memState.activeContractSet memState.cgp validatedTransactions pkHash
        |> Result.get
        |> requestId.reply<Block>
    | GetBlock (mainChain,blockHash) ->
        let isValid (header:ExtendedBlockHeader.T) = (not mainChain) || header.status = ExtendedBlockHeader.MainChain

        match BlockRepository.tryGetHeader session blockHash with
        | Some header when isValid header  ->
            BlockRepository.getFullBlock session header
            |> Some
            |> requestId.reply<Types.Block option>
        | _ -> requestId.reply<Types.Block option> None
    | GetBlockByNumber blockNumber ->
        if blockNumber > state.tipState.tip.header.blockNumber || blockNumber = 0ul then
            requestId.reply<Types.Block option> None
        else
            let rec findBlock (header:ExtendedBlockHeader.T) =
                if header.header.blockNumber = blockNumber then
                    BlockRepository.getFullBlock session header
                else
                    BlockRepository.getHeader session header.header.parent
                    |> findBlock

            findBlock state.tipState.tip
            |> Some
            |> requestId.reply<Types.Block option>

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
    | GetActiveContracts ->
        ActiveContractSet.getContracts state.memoryState.activeContractSet
        |> Seq.map (fun contract ->
            {
                contractId = contract.contractId
                expiry = contract.expiry
                code = contract.code
            }:ActiveContract)
        |> List.ofSeq
        |> requestId.reply<ActiveContract list>
    | GetActiveContract contractId ->
        ActiveContractSet.tryFind contractId state.memoryState.activeContractSet
        |> Option.map (fun contract ->
            {
                contractId = contract.contractId
                expiry = contract.expiry
                code = contract.code
            }:ActiveContract)
        |> requestId.reply<ActiveContract option>
    | GetHeaders ->
        let rec getHeaders tip headers =
            let header = BlockRepository.getHeader session tip

            if header.header.blockNumber = 1ul then
                header.header :: headers
            else
                getHeaders header.header.parent (header.header :: headers)

        getHeaders state.tipState.tip.hash []
        |> requestId.reply<BlockHeader list>
    | GetMempool ->
        MemPool.toList state.memoryState.mempool
        |> List.map (fun ex-> ex.txHash,ex.tx)
        |> requestId.reply<List<Hash.Hash * Transaction>>
    | GetBlockChainInfo ->
        let tip = state.tipState.tip.header

        let difficulty =
            if state.tipState.tip.header.blockNumber = 0ul then
                1.0
            else
                let mutable shift = (tip.difficulty >>> 24) &&& 0xfful;
                let mutable diff =
                    float 0x0000ffff / float (tip.difficulty &&& 0x00fffffful)

                while shift < 29ul do
                    diff <- diff * 256.0
                    shift <- shift + 1ul

                while shift > 29ul do
                    diff <- diff / 256.0
                    shift <- shift - 1ul

                diff

        let syncing = InitialBlockDownload.isActive state.initialBlockDownload

        {
            chain = chain.name
            blocks = state.tipState.tip.header.blockNumber
            headers =
                InitialBlockDownload.getPeerHeader state.initialBlockDownload
                |> Option.map (fun bh -> bh.blockNumber)
                |> Option.defaultValue state.headers
            medianTime = EMA.earliest state.tipState.ema
            difficulty = difficulty
            initialBlockDownload = syncing
            tipBlockHash = Block.hash state.tipState.tip.header
        }
        |> requestId.reply<BlockchainInfo>

    | GetTransaction txHash ->
        match MemPool.getTransaction txHash state.memoryState.mempool with
        | Some ex ->
            Some (ex.tx,0ul)
            |> requestId.reply<(Transaction*uint32) option>
        | None ->
            TransactionRepository.tryGetTransaction session txHash
            |> Option.bind (fun tx ->
                TransactionRepository.tryGetTransactionBlock session txHash
                |> Option.map (fun block -> tx,block))
            |> function
            | Some (ex,block) ->
                let confirmations = (state.tipState.tip.header.blockNumber - block.header.blockNumber) + 1ul

                Some (ex.tx,confirmations)
                |> requestId.reply<(Transaction*uint32) option>
            | None ->
                requestId.reply<(Transaction*uint32) option> None

    | CheckTransaction ex ->
        TransactionValidation.validateBasic ex.tx
        >>= fun _ -> TransactionValidation.validateInContext chain
                        (TransactionHandler.getUTXO session)
                        session.context.contractPath
                        (state.tipState.tip.header.blockNumber + 1ul)
                        timestamp
                        state.memoryState.activeContractSet
                        state.memoryState.contractCache
                        state.memoryState.utxoSet
                        (TransactionHandler.getContractState session)
                        state.memoryState.contractStates
                        ex
        <@> (fun _ -> ex.txHash)
        |> requestId.reply<Result<Hash.Hash,ValidationError.ValidationError>>
    | GetTotalZP ->
        [2ul..state.tipState.tip.header.blockNumber]
        |> List.fold (fun sum blockNumber -> sum + blockReward blockNumber state.tipState.cgp.allocation) (20_000_000UL * 100_000_000UL)
        |> requestId.reply
    | GetBlockReward blockNumber ->
        blockReward blockNumber state.tipState.cgp.allocation
        |> requestId.reply
    | GetCGP ->
        state.tipState.cgp
        |> requestId.reply<CGP.T>
    | GetCgpHistory ->
        let currentInterval = state.tipState.tip.header.blockNumber / chain.intervalLength

        let getBlockState blockNumber =
            let rec findBlock (header:ExtendedBlockHeader.T) =
                if header.header.blockNumber = blockNumber then
                    BlockRepository.getBlockState session header.hash
                else
                    BlockRepository.getHeader session header.header.parent
                    |> findBlock

            findBlock state.tipState.tip

        let rec getCGP interval cgpList =
            if interval = 0ul then
               cgpList
            else
                let blockState:BlockState.T = getBlockState (interval * chain.intervalLength)

                (blockState.cgp :: cgpList)
                |> getCGP (interval - 1ul)

        []
        |> getCGP currentInterval
        |> requestId.reply<CGP.T list>
    |> ignore
           
    logEndAction timestamp "request" request

    ret state

let handleEvent _ _ _ state =
    ret state

let tick _ _ timestamp state = effectsWriter {
    let! ibd = InitialBlockDownload.tick timestamp state.initialBlockDownload

    return {state with initialBlockDownload = ibd}
}
