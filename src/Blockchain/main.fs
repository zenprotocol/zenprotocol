module Blockchain.Main

open Blockchain
open FsNetMQ
open FSharp.Control
open Infrastructure
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus
open State
open Logary.Message

let main dataPath chainParams busName wipe =
    let dataPath = Platform.combine dataPath "blockchaindb"

    if wipe then
        eventX "Wiping blockchain database"
        |> Log.info
        if System.IO.Directory.Exists dataPath then
                System.IO.Directory.Delete (dataPath,true)

    Actor.create<Command,Request,Event,State> busName serviceName (fun poller sbObservable ebObservable  ->
        let publisher = EventBus.Publisher.create<Event> busName
        let client = ServiceBus.Client.create busName
        let ticker = Timer.create 1000<milliseconds>

        let sbObservable =
            sbObservable
            |> Observable.map (fun message ->
                match message with
                | ServiceBus.Agent.Command c -> Handler.handleCommand chainParams c
                | ServiceBus.Agent.Request (requestId, request) ->
                    Handler.handleRequest chainParams requestId request)

        let ebObservable =
            ebObservable
            |> Observable.map Handler.handleEvent

        let tickerObservable =
            Poller.addTimer poller ticker
            |> Observable.map (fun _ ->
                Handler.tick chainParams)

        let databaseContext = DatabaseContext.create dataPath

        let tip, acs, ema, contractCache, cgp =
            use session = DatabaseContext.createSession databaseContext
            match BlockRepository.tryGetTip session with
            | Some (tip,ema,cgp) ->
                eventX "Loading tip from db #{blockNumber} {blockHash}"
                >> setField "blockNumber" tip.header.blockNumber
                >> setField "blockHash" (Hash.toString tip.hash)
                |> Log.info

                let acs = ActiveContractSetRepository.get session

                tip,
                acs,
                ema,
                Seq.fold (fun contractCache contract ->
                    ContractCache.add contract contractCache)
                    ContractCache.empty (ActiveContractSet.getContracts acs),
                cgp

            | None ->
                eventX "No tip in db"
                |> Log.info

                ExtendedBlockHeader.empty,
                ActiveContractSet.empty,
                EMA.create chainParams,
                ContractCache.empty,
                CGP.empty

        let tipState =
            {
                activeContractSet=acs
                ema=ema
                tip=tip
                cgp=cgp
            }

        let memoryState =
            {
                activeContractSet=acs
                utxoSet=UtxoSet.asDatabase
                mempool=MemPool.empty
                orphanPool=OrphanPool.create ()
                contractCache=contractCache
                contractStates=ContractStates.asDatabase
                invalidTxHashes = Set.empty
            }

        let state =
            {
                memoryState = memoryState;
                tipState = tipState;
                headers=tip.header.blockNumber
                initialBlockDownload = InitialBlockDownload.Inactive
            }

        let observable =
            Observable.merge sbObservable ebObservable
            |> Observable.merge tickerObservable
            |> Observable.scan (fun state handler ->
                use session = DatabaseContext.createSession databaseContext
                let effectWriter = handler session (Timestamp.now ()) state
                DataAccess.Session.commit session.session
                let result = EffectsWriter.run effectWriter publisher client

                result
                ) state

        Disposables.toDisposable databaseContext, observable
    )