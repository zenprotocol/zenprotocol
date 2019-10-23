module Blockchain.InitialBlockDownload

open Blockchain
open Consensus.Types
open Consensus
open Blockchain.EffectsWriter
open Infrastructure
open Infrastructure.Timestamp
open Logary.Message

let treshhold = 500ul

let LastTickTimeout = 3UL * Second 

let NoResponseTimeout = 10UL * Second

let maxDownloading = 20

let MaxHeaders = 2000

type Syncing = {
    peerId: byte[];
    peerTipHash: Hash.Hash;
    peerTipHeader: BlockHeader;
}

type Downloading = {
    pending: Hash.Hash list;
    inprogress: (Hash.Hash*Timestamp) list
    isLastBatch: bool
}

type T =
    | Inactive
    | GettingHeaders of Syncing * requestTime:Timestamp * lastTickTime:Timestamp
    | DownloadingBlocks of Syncing * Downloading * lastTickTime:Timestamp

let isActive ibd =
    match ibd with
    | Inactive -> false
    | _ -> true

let shouldStartInitialBlockDownload (tip:BlockHeader) (peerTip:BlockHeader) = peerTip.blockNumber >= tip.blockNumber + treshhold

let getPeerHeader ibd =
    match ibd with
    | Inactive -> None
    | DownloadingBlocks (syncing,_,_)
    | GettingHeaders (syncing,_,_) -> Some syncing.peerTipHeader

let start timestamp tipHash (tipHeader:BlockHeader) peerId peerTipHeader = effectsWriter {
    let peerTipHash = Block.hash peerTipHeader

    eventX "Starting initial block download from {tip} to {peerTip}"
    >> setField "tip" tipHeader.blockNumber
    >> setField "peerTip" peerTipHeader.blockNumber
    |> Log.info

    // TODO: send 20 last blocks instead of only the last block to avoid case of starting from genesis due to reorg

    do! getHeaders peerId [tipHash] peerTipHash

    return GettingHeaders ({peerId=peerId;peerTipHash=peerTipHash;peerTipHeader=peerTipHeader}, timestamp, timestamp)
}

let processHeaders chain session timestamp peerId (headers:BlockHeader list) ibd = effectsWriter {
    let processHeaders' syncing = effectsWriter {
        eventX "Processing #{headers} headers from #{from} to #{to}"
        >> setField "headers" (List.length headers)
        >> setField "from" (List.head headers).blockNumber
        >> setField "to" (List.last headers).blockNumber
        |> Log.info

        // check if any of the block is invalid, if so we quit the IBD process and ask for tips
        let allValid = List.forall (BlockValidation.Header.validate chain >> Result.isOk) headers

        if not allValid then
            eventX "Process headers failed, not all headers are valid. Disconnect node and sending request again"
            |> Log.info

            // We need to cancel the process and ask for all tips again
            do! disconnectPeer syncing.peerId
            do! getTipsFromAllPeers

            return Inactive
        else
            let missingBlocks =
                List.map (fun header -> Block.hash header, header) headers
                |> List.reject (fun (hash,_) -> BlockRepository.contains session hash)

            if List.isEmpty missingBlocks then
                // Are we done?
                if List.length headers < MaxHeaders || (List.last headers).blockNumber >= syncing.peerTipHeader.blockNumber then
                    // Initial block download is done

                    eventX "Initial Block Download is done"
                    |> Log.info

                    do! getTipsFromAllPeers

                    return Inactive
                else
                    eventX "Initial Block download getting next batch of headers"
                    |> Log.info
                    do! getHeaders syncing.peerId [(List.last headers |> Block.hash)] syncing.peerTipHash

                    return GettingHeaders (syncing, timestamp, timestamp)
            else
                let pending, blocksToDownload =
                    if List.length missingBlocks <= maxDownloading then
                        [], missingBlocks
                    else
                        List.skip maxDownloading missingBlocks, List.take maxDownloading missingBlocks

                for (hash,block) in blocksToDownload do
                    do! getBlockFrom peerId hash

                let inprogress =
                    List.unzip blocksToDownload |> fst
                    |> List.map (fun blockHash -> blockHash,timestamp)

                let pending = List.unzip pending |> fst

                let isLastBatch = List.length headers < MaxHeaders || (List.last headers).blockNumber >= syncing.peerTipHeader.blockNumber

                return DownloadingBlocks (syncing, {inprogress=inprogress;pending=pending;isLastBatch=isLastBatch}, timestamp)
    }

    if not <| List.isEmpty headers then
        match ibd with
        | DownloadingBlocks _
        | Inactive -> return ibd
        | GettingHeaders (syncing,_,_) when syncing.peerId <> peerId -> return ibd
        | GettingHeaders (syncing,_,_) -> return! processHeaders' syncing
    else
        return ibd
}

// Called when valid block received
let received timestamp blockHash ibd = effectsWriter {
    match ibd with
    | Inactive
    | GettingHeaders _ -> return ibd
    | DownloadingBlocks (syncing,downloading,_) ->
        let findBlock = (fst >> (=) blockHash)

        if List.exists findBlock downloading.inprogress then

            let inprogress = List.reject findBlock downloading.inprogress

            match downloading.pending, inprogress with
            | next :: pending, inprogress ->
                do! getBlockFrom syncing.peerId next

                let inprogress = (next,timestamp) :: inprogress

                return DownloadingBlocks(syncing, {downloading with pending=pending;inprogress=inprogress}, timestamp)
            | [], [] ->

                // requesting more headers

                if downloading.isLastBatch then
                    // Initial block download is done

                    eventX "Initial Block Download is done"
                    |> Log.info

                    do! getTipsFromAllPeers

                    return Inactive
                else
                    eventX "Initial Block download getting next batch of headers"
                    |> Log.info

                    do! getHeaders syncing.peerId [blockHash] syncing.peerTipHash

                    return GettingHeaders (syncing, timestamp, timestamp)

            | _ ->
                return DownloadingBlocks (syncing, {downloading with inprogress = inprogress}, timestamp)
        else
            return ibd
}

let invalid timestamp blockHash ibd = effectsWriter {
    match ibd with
    | Inactive
    | GettingHeaders _ -> return ibd
    | DownloadingBlocks (syncing, downloading, _) ->

        if List.exists (fst >> (=) blockHash) downloading.inprogress then
            eventX "Invalid block. Disconnect node and restart Initial Block Download"
            |> Log.info

            // we cancel the IBD and re-ask for a tip
            do! disconnectPeer syncing.peerId
            do! getTipsFromAllPeers

            return Inactive
        else
            return ibd
}

let tick now ibd = effectsWriter {
    let isTimedOut timeout timestamp = now > timestamp + timeout

    match ibd with
    | Inactive -> return ibd
    | GettingHeaders (syncing, timestamp, lastTickTime) ->
        // We cannot trust the timestamp check if the lastTick time is the one timed out
        if not <| isTimedOut LastTickTimeout lastTickTime  && isTimedOut NoResponseTimeout timestamp then
            do! disconnectPeer syncing.peerId
            do! getTipsFromAllPeers

            eventX "Initial Block Download stopped as peer is not respoding, finding another peer"
            |> Log.info

            return Inactive
        else
            return GettingHeaders(syncing, timestamp, now)
    | DownloadingBlocks (syncing, downloading, lastTickTime) ->
        if isTimedOut LastTickTimeout lastTickTime then
            return DownloadingBlocks (syncing, downloading, now)
        else
            let anyTimedout = List.exists (snd >> isTimedOut NoResponseTimeout) downloading.inprogress

            // TODO: instead of timed out we might want to try again

            if anyTimedout then
                eventX "Initial Block Download stopped as peer is not respoding, finding another peer"
                |> Log.info

                do! disconnectPeer syncing.peerId
                do! getTipsFromAllPeers

                return Inactive
            else
                return DownloadingBlocks (syncing, downloading, now)
}

let getHeaders (chainParams:Chain.ChainParameters) session peerId from endHash =
    let rec getHeaders (fromHeader:ExtendedBlockHeader.T) toHash left acc =
        if fromHeader.hash = toHash || left = 0 then
            acc |> List.rev
        else
            let child =
                BlockRepository.getBlockChildren session fromHeader
                |> Seq.filter (fun header -> header.status = ExtendedBlockHeader.MainChain)
                |> Seq.tryHead

            match child with
            | None -> acc |> List.rev
            | Some child -> getHeaders child toHash (left - 1) (child.header :: acc)

    let fromGenesis toHash = effectsWriter {
        match BlockRepository.tryGetGenesisHeader session with
        | None ->
            // we don't have genesis, send nothing
            return ()
        | Some genesis ->
            let headers = getHeaders genesis toHash (MaxHeaders - 1) [genesis.header]
            do! sendHeaders peerId headers

            return ()
    }

    // TODO: we should actually receive a list of start points, try to find at least one

    effectsWriter {
        let getFromHeader =
            List.choose (fun hash ->
                match BlockRepository.tryGetHeader session hash with
                | Some from when from.status = ExtendedBlockHeader.MainChain -> Some from
                | _ -> None)
            >> List.tryHead

        match from with
        | [] -> return ()
        | [hash] when hash = Hash.zero -> do! fromGenesis endHash
        | from ->
            match getFromHeader from with
            | None -> do! fromGenesis endHash
            | Some from ->
                let headers = getHeaders from endHash MaxHeaders []
                do! sendHeaders peerId headers

    }