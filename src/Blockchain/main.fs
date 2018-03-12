module Blockchain.Main

open FsNetMQ
open FSharp.Control
open Infrastructure
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus
open State

let main dataPath chainParams busName =
    Actor.create<Command,Request,Event,State> busName serviceName (fun poller sbObservable ebObservable  ->
        let publisher = EventBus.Publisher.create<Event> busName
        let client = ServiceBus.Client.create busName

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

        let databaseContext = DatabaseContext.create dataPath

        let tip,acs,ema =
            use session = DatabaseContext.createSession databaseContext
            match BlockRepository.tryGetTip session with
            | Some (tip,acs,ema) ->
                Log.info "Loading tip from db #%d %A" tip.header.blockNumber tip.hash

                tip,acs,ema
            | None ->
                Log.info "No tip in db"
                ExtendedBlockHeader.empty,ActiveContractSet.empty,EMA.create chainParams

        let tipState =
            {
                activeContractSet=acs
                ema=ema
                tip=tip
            }

        let memoryState =
            {
                activeContractSet=acs
                utxoSet=UtxoSet.asDatabase
                mempool=MemPool.empty
                orphanPool=OrphanPool.create ()
            }

        let state =
            {
                memoryState = memoryState;
                tipState = tipState;
                blockRequests= Map.empty;
                headers=tip.header.blockNumber
            }

        let observable =
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun state handler ->
                use session = DatabaseContext.createSession databaseContext
                let effectWriter = handler session (Timestamp.now ()) state
                DataAccess.Session.commit session.session
                let result = EffectsWriter.run effectWriter publisher client

                result
                ) state

        Disposables.toDisposable databaseContext, observable
    )