open System
open FsNetMQ
open FSharp.Configuration
open Argu
open Infrastructure
open Consensus
open Logary.Message

module Actor = FsNetMQ.Actor

type Wipe =
    | Full

#if DEBUG
[<Literal>]
let Localhost = "127.0.0.1"
#endif

[<NoAppSettings>]
type Argument =
    | Test
    | Api of string
    | Bind of string
    | Ip of string
    | Wipe of full:Wipe option
    | Miner of threads:int option
#if DEBUG
    // non-zero indexed nodes act as outbound nodes which connect to the zero-indexed inbound node
    | Local of int
#endif
    | [<Hidden>] Seed
    | AddressDB
    | Data_Path of string
    | Service_Bus of string
    | Publisher of string
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Api _ -> "enable api and set bind address"
                | Bind _ -> "set the address the node should listen on"
                | Test -> "use testnet"
                | Ip _ -> "specify the IP the node should relay to other peers"
                | Wipe _ -> "wipe database, specify full to wipe the wallet's private key"
                | Miner _ -> "enable miner and optionally specify number of threads"
                | Seed -> "run node as a seed"
                | AddressDB -> "enable the AddressDB module"
                | Data_Path _ -> "set the data folder path"
                | Service_Bus _ -> "expose the service bus over zeromq address"
                | Publisher _ -> "expose the publisher over zeromq address"
#if DEBUG
                | Local _ -> "local mode, used for debugging. specify zero value for host, other for client"
#endif


type Config = YamlConfig<"scheme.yaml">

let busName = "main"

let createBroker serviceBusAddress publisherBusAddress =
     Actor.create (fun shim ->
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim

        use sbBroker = ServiceBus.Broker.create poller busName serviceBusAddress
        use evBroker = EventBus.Broker.create poller busName publisherBusAddress

        Actor.signal shim
        Poller.run poller
)

let getChain (config:Config) =
    match config.chain with
    | "main" -> Chain.Main
    | "test" -> Chain.Test
    | _ -> Chain.Local

[<EntryPoint>]
let main argv =
    use logary = Log.create

    eventX "Node running... press CTRL+C to exit"
    |> Log.info

    match Platform.runNative ZFStar.z3Name ["--help"] with
    | Ok _ ->
        eventX "Z3 check passed"
        |> Log.info
    | Error error ->
        eventX "Failed to run z3. Exit node.\n{error}"
        >> setField "error" error
        |> Log.error

        System.Environment.FailFast(sprintf "Failed to run z3.exe\n%s" error, null)

    // Check if 64bit machine
    if Platform.platform = PlatformID.Win32NT && not Platform.is64bit  then
        System.Environment.FailFast("Zen-node can only run on 64 bit OS", null)

    if Platform.isUnix then
        match Platform.monoVersion with
        | None ->
            eventX "Please install mono."
            |> Log.error

            System.Environment.FailFast("Please install mono", null)
        | Some version ->
            if version >= Version(5,10,0) then
                eventX "Mono check passed"
                |> Log.info
            else
                eventX "Old version of mono, please upgrade"
                |> Log.error

                System.Environment.FailFast("Please install mono", null)

    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)

    let config = new Config()

    let parser = ArgumentParser.Create<Argument>(programName = "zen-node.exe", errorHandler = errorHandler)
    let results = parser.Parse argv

    let mutable wipe = false
    let mutable wipeFull = false
    let mutable seed = false
    let mutable addressDb = false
    let mutable serviceBusAddress = None
    let mutable publisherAddress = None
    let mutable chain = Chain.Main

    if List.contains Test (results.GetAllResults()) then
        config.Load("test.yaml")
    else 
        config.Load("main.yaml")

    chain <- getChain config

    List.iter (fun arg ->
        match arg with
#if DEBUG
        | Local idx ->
            let getEndpoint port idx = sprintf "%s:%i" Localhost (port + idx)

            chain <- Chain.Local
            config.dataPath <- sprintf "./data/local/%i" idx
            config.chain <- "local"
            config.externalIp <- Localhost
            config.bind <- getEndpoint 10000 idx
            config.api.bind <- getEndpoint 20000 idx
            config.seeds.Clear ()

            if idx <> 0 then config.seeds.Add (getEndpoint 10000 0)
#endif
        | Api address ->
            config.api.enabled <- true
            config.api.bind <- address
        | Bind address ->
            config.bind <- address
            config.listen <- true
        | Ip ip ->
            config.externalIp <- ip
        | Wipe full ->
            wipe <- true
            wipeFull <- Option.isSome full
        | Miner None ->
            config.miner.enabled <- true
        | Miner (Some threads) ->
            config.miner.enabled <- true
            config.miner.threads <- threads
        | Seed ->
            seed <- true
            config.listen <- true
        | AddressDB ->
            addressDb <- true
        | Data_Path dataPath ->
            config.dataPath <- dataPath
        | Publisher address ->
            publisherAddress <- Some address
        | Service_Bus address ->
            serviceBusAddress <- Some address
        | _ ->
            ()
    ) (results.GetAllResults())

    let chainParams = Chain.getChainParameters chain
    let dataPath = Platform.combine config.dataPath config.chain
    
    eventX "DB is stored at: {dataPath}"
    >> setField "dataPath" (Platform.getAbsolutePath dataPath)
    |> Log.info
    
    let seeds =
        config.seeds
        |> Seq.choose (Endpoint.parseIp config.bind)

    use brokerActor = createBroker serviceBusAddress publisherAddress
    
    use apiActor =
        if config.api.enabled then
            Api.Main.main chain busName config.api.bind
            |> Disposables.toDisposable
        else
            Disposables.empty
            
    use blockchainActor = Blockchain.Main.main dataPath chainParams busName wipe

    use networkActor =
        Network.Main.main dataPath busName chainParams config.externalIp config.listen config.bind seeds wipe seed

    use walletActor =
        if wipeFull then Wallet.Main.Full elif wipe then Wallet.Main.Reset else Wallet.Main.NoWipe
        |> Wallet.Main.main dataPath busName chain

    use addressDbActor =
        if addressDb then
            if wipeFull then AddressDB.Main.Full elif wipe then AddressDB.Main.Reset else AddressDB.Main.NoWipe
            |> AddressDB.Main.main dataPath busName chain
            |> Disposables.toDisposable
        else
            Disposables.empty

    use minerActor =
        if config.miner.enabled then
            Miner.Main.main busName config.miner.threads
            |> Disposables.toDisposable
        else
            Disposables.empty


#if DEBUG
    if chain = Chain.Local then
//        let block = Consensus.Block.createGenesis Chain.localParameters [Transaction.toExtended Consensus.Tests.Helper.rootTx] (0UL,0UL)
//        printfn "%A" (Block.hash block.header)
//        printfn "----"
//        printfn "%A" (Serialization.Block.serialize block |> FsBech32.Base16.encode)

        let block =
            "000000000000000000000000000000000000000000000000000000000000000000000000000000013ca83bcc8483b5a8706a8fed28e4ec64952d7d6b65624c8e8026f48cf177176700000160e073fe8f20ffffff0000000000000000000000000000000003b01098756bcf637bef2a161bd49412cad0a10adf12e64a694c41f9c5b971642029f0999def953f2a14ad6c143e2a0ebf3b4f794d8b17fc1203c12365427c09d3be653064be80f760b9d471dc9afbac2b24236c9f2eb0f08b7427942852dc780201000000000001022030759b07ca01caf8e524fc279946a1e96afc3546ee5f1fd4a1cfaf644763c2b4002c010000"
            |> Block.fromHex
            |> Option.get

        use client = ServiceBus.Client.create busName

        Messaging.Services.Blockchain.validateMinedBlock client block
#endif

    use event = new Threading.ManualResetEvent(false)

    Console.CancelKeyPress.Add (fun e ->
        e.Cancel <- true
        eventX "Closing..."
        |> Log.info
        event.Set() |> ignore
    )

    event.WaitOne() |> ignore

    0