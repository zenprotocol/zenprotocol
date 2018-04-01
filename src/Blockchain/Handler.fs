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

let handleCommand chainParams command session timestamp (state:State) =
    let contractPath = session.context.contractPath

    match command with
    | ValidateTransaction tx ->
        effectsWriter {
            let! memoryState =
                TransactionHandler.validateTransaction chainParams session contractPath state.tipState.tip.header.blockNumber
                    tx state.memoryState
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
        BlockHandler.validateBlock chainParams contractPath session timestamp block false state
    | ValidateMinedBlock block ->
        BlockHandler.validateBlock chainParams contractPath session timestamp block true state
    | HandleTip (peerId,header) ->
        BlockHandler.handleTip chainParams session peerId header state
    | ValidateNewBlockHeader (peerId, header) ->
        BlockHandler.handleNewBlockHeader chainParams session peerId header state
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
    | RequestHeaders (peerId, blockHash, numberOfHeaders) ->
        let rec getHeaders blockHash headers left =
            if left = 0us then
                headers
            else
                match BlockRepository.tryGetHeader session blockHash with
                | None -> headers
                | Some exHeader ->
                    if blockHash <> chainParams.genesisHash then
                        getHeaders exHeader.header.parent (exHeader.header :: headers) (left - 1us)
                    else
                        (exHeader.header :: headers)

        effectsWriter {

            let numberOfHeaders = if numberOfHeaders > 500us then 500us else numberOfHeaders

            let headers = getHeaders blockHash [] numberOfHeaders

            do! sendHeaders peerId headers

            return state
        }
    | HandleHeaders (peerId,headers) ->
       BlockHandler.handleHeaders chainParams session peerId headers state

let handleRequest chain (requestId:RequestId) request session timestamp state =
    match request with
    | ExecuteContract (cHash, command, data, returnAddress, txSkeleton) ->
        TransactionHandler.executeContract session txSkeleton cHash command data returnAddress state.memoryState
        |> requestId.reply
    | GetBlockTemplate pkHash ->
        let memState, validatedTransactions = BlockTemplateBuilder.makeTransactionList chain session state
        let block = Block.createTemplate chain state.tipState.tip.header (Timestamp.now ()) state.tipState.ema memState.activeContractSet validatedTransactions pkHash

        requestId.reply block
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
    | GetActiveContracts ->
        ActiveContractSet.getContracts state.memoryState.activeContractSet
        |> Seq.map (fun contract ->
            {
                contractHash = contract.hash
                expiry = contract.expiry
                code = contract.code
            })
        |> List.ofSeq
        |> requestId.reply<ActiveContract list>
    | GetHeaders ->
        let rec getHeaders tip headers =
            let header = BlockRepository.getHeader session tip

            if tip = chain.genesisHash then
                header.header :: headers
            else
                getHeaders header.header.parent (header.header :: headers)

        let headers = getHeaders state.tipState.tip.hash []

        requestId.reply headers

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
        {
            chain = chain.name
            blocks = state.tipState.tip.header.blockNumber
            headers = state.headers
            medianTime = EMA.earliest state.tipState.ema
            difficulty = difficulty
        }
        |> requestId.reply

    ret state

let handleEvent event session timestamp state =
    ret state
