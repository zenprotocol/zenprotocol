module Blockchain.Main

open Blockchain
open FsNetMQ
open FSharp.Control
open Infrastructure
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus
open Consensus.Chain
open State
open Logary.Message

let wipeDirectory path =
    if System.IO.Directory.Exists path then
        System.IO.Directory.Delete(path, true)

let initializeDatabase dataPath =
    let session = DatabaseContext.createSession dataPath

    match DataAccess.SingleValue.tryGet dataPath.dbVersion session.session with
    | None ->
        DataAccess.SingleValue.put dataPath.dbVersion session.session DatabaseContext.DbVersion
    | Some 0 | Some 1 ->
        eventX "Found an old DbVersion, update in process..."
        |> Log.warning
        DatabaseContext.updateVersion dataPath session.session
    | Some DatabaseContext.DbVersion -> ()
    | Some version ->
        failwithf "Blockchain: wrong db version, expected %d but got %d" DatabaseContext.DbVersion version

    session

let loadOrInitializeTipState session chainParams =
    match BlockRepository.tryGetTip session with
    | Some tipInfo -> 
        let tip, ema, cgp = match tipInfo with
                            | (t, e, c) -> t, e, c

        eventX "Loading tip from db #{blockNumber} {blockHash}"
        >> setField "blockNumber" tip.header.blockNumber
        >> setField "blockHash" (Hash.toString tip.hash)
        |> Log.info

        let acs = ActiveContractSetRepository.get session
        let contractCache = 
            Seq.fold (fun contractCache contract ->
                    ContractCache.add contract contractCache)
                    ContractCache.empty (ActiveContractSet.getContracts acs)

        tip, acs, ema, contractCache, cgp

    | None ->
        eventX "No tip in db"
        |> Log.info

        ExtendedBlockHeader.empty,
        ActiveContractSet.empty,
        EMA.create chainParams,
        ContractCache.empty,
        CGP.empty


let main dataPath chainParams busName wipe =
    let dataPath = Platform.combine dataPath "blockchaindb"
    
    if wipe then
        eventX "Wiping blockchain database"
        |> Log.info
        wipeDirectory dataPath

    let databaseContext = DatabaseContext.create dataPath

    Actor.create<Command, Request, Event, State> busName serviceName (fun poller sbObservable ebObservable  ->
        let publisher = EventBus.Publisher.create<Event> busName
        let client = ServiceBus.Client.create busName
        let ticker = Timer.create 1000<milliseconds>

        let handleServiceBus message =
            match message with
            | ServiceBus.Agent.Command c -> Handler.handleCommand chainParams c
            | ServiceBus.Agent.Request (requestId, request) -> Handler.handleRequest chainParams requestId request
        
        let sbObservable = sbObservable |> Observable.map handleServiceBus
        let ebObservable = ebObservable |> Observable.map Handler.handleEvent
        let tickerObservable = Poller.addTimer poller ticker |> Observable.map (fun _ -> Handler.tick chainParams)

        use session = initializeDatabase databaseContext
        
        let tip, acs, ema, contractCache, cgp = loadOrInitializeTipState session chainParams
        
        let tipState = {
            activeContractSet = acs
            ema = ema
            tip = tip
        }

        let memoryState = {
            activeContractSet = acs
            utxoSet = UtxoSet.asDatabase
            mempool = MemPool.empty
            orphanPool = OrphanPool.create ()
            contractCache = contractCache
            contractStates = ContractStates.asDatabase
            invalidTxHashes = Set.empty
        }

        let state = {
            memoryState = memoryState
            tipState = tipState
            headers = tip.header.blockNumber
            initialBlockDownload = InitialBlockDownload.Inactive
            cgp = cgp
        }

        let updateState state handler =
            use session = DatabaseContext.createSession databaseContext
            let effectWriter = handler session (Timestamp.now ()) state
            DataAccess.Session.commit session.session
            EffectsWriter.run effectWriter publisher client

        let observable =
            Observable.merge sbObservable ebObservable
            |> Observable.merge tickerObservable
            |> Observable.scan updateState state

        Disposables.toDisposable databaseContext, observable
    )
