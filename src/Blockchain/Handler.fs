module Blockchain.Handler

open Infrastructure
open Infrastructure.Writer
open Infrastructure.ServiceBus.Agent
open Messaging.Services
open Messaging.Events
open Consensus
open Blockchain
open Blockchain.EffectsWriter
open Consensus.Types
open State
open DatabaseContext
open Logary.Message


let handleCommand chainParams command session timestamp (state:State) =
    let contractPath = session.context.contractPath

    match command with
    | ValidateTransaction tx ->
        effectsWriter {
            let! memoryState =
                TransactionHandler.validateTransaction chainParams session contractPath state.tipState.tip.header.blockNumber
                    timestamp tx state.memoryState
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
                eventX "Handling mempool message"
                |> Log.info

                if not (MemPool.containsTransaction txHash state.memoryState.mempool) then
                    do! getTransaction peerId txHash
                else
                    return ()
            }

        let writer =
            List.map handleTxHash txHashes
            |> List.fold (fun state writer -> Writer.bind state (fun () -> writer)) (Writer.ret ())

        Writer.bind writer (fun () -> Writer.ret state)
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
    | HandleNewTransaction (peerId, txHash) ->
        effectsWriter {
            if not (MemPool.containsTransaction txHash state.memoryState.mempool) then
                do! getTransaction peerId txHash

            return state
        }

let handleRequest chain (requestId:RequestId) request session timestamp state =
    match request with
    | ExecuteContract (contractId, command, sender, messageBody, txSkeleton) ->
        TransactionHandler.executeContract session txSkeleton timestamp contractId command sender messageBody state false
        |> requestId.reply
    | GetBlockTemplate pkHash ->
        let memState, validatedTransactions = BlockTemplateBuilder.makeTransactionList chain session state timestamp
        let block = Block.createTemplate chain state.tipState.tip.header timestamp state.tipState.ema memState.activeContractSet validatedTransactions pkHash

        requestId.reply block
    | GetBlock blockHash ->
        match BlockRepository.tryGetHeader session blockHash with
        | Some header ->
            BlockRepository.getFullBlock session header
            |> Some
            |> requestId.reply<Types.Block option>
        | None -> requestId.reply<Types.Block option> None
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
        |> requestId.reply
    | GetHeaders ->
        let rec getHeaders tip headers =
            let header = BlockRepository.getHeader session tip

            if tip = chain.genesisHash then
                header.header :: headers
            else
                getHeaders header.header.parent (header.header :: headers)

        let headers = getHeaders state.tipState.tip.hash []

        requestId.reply headers

    | GetMempool ->
        let txs = MemPool.toList state.memoryState.mempool
        requestId.reply txs

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
        |> requestId.reply

    | GetTransaction txHash ->
        match MemPool.getTransaction txHash state.memoryState.mempool with
        | Some tx ->
            Some (tx,0ul)
            |> requestId.reply<(Transaction*uint32) option>
        | None ->
            TransactionRepository.tryGetTransaction session txHash
            |> Option.bind (fun tx ->
                TransactionRepository.tryGetTransactionBlock session txHash
                |> Option.map (fun block -> tx,block))
            |> function
            | Some (tx,block) ->
                let confirmations = (state.tipState.tip.header.blockNumber - block.header.blockNumber) + 1ul

                Some (tx,confirmations)
                |> requestId.reply<(Transaction*uint32) option>
            | None ->
                requestId.reply<(Transaction*uint32) option> None

    ret state

let handleEvent event session timestamp state =
    ret state

let tick chain session timestamp state = effectsWriter {
    let! ibd = InitialBlockDownload.tick timestamp state.initialBlockDownload

    return {state with initialBlockDownload = ibd}
}
