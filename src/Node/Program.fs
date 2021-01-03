open System
open FsNetMQ
open FSharp.Configuration
open Argu
open Infrastructure
open Consensus
open Consensus.Chain
open Endpoint
open Logary.Message

module Actor = FsNetMQ.Actor

type Wipe =
    | Full

[<Literal>]
let Localhost = "127.0.0.1"
[<Literal>]
let LocalhostString = "localhost"

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
    | ConnectWallet
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
                | ConnectWallet _ -> "connect the new desktop wallet"
#if DEBUG
                | Local _ -> "local mode, used for debugging. specify zero value for host, other for client"
#endif

let busName = "main"

type MainConfig =
    {
        chainParams: ChainParameters
        wipe: bool
        wipeFull: bool
        seed: bool
        wallet: bool
        addressDb: bool
        miner: bool
        api: bool
        serviceBusAddress: string option
        publisherAddress: string option
        chain: Chain.Chain
        seeds: Collections.Generic.IList<string>
        dataPath: string
        chainPath: string
        bind: string
        apiBind: string
        minerThreads: int
        externalIp: string
        listen: bool
    }
#if DEBUG
module Local =
    let addGenesis (config:MainConfig) =
        if config.chain = Chain.Local then
            let block =
                "000000000000000000000000000000000000000000000000000000000000000000000000000000013ca83bcc8483b5a8706a8fed28e4ec64952d7d6b65624c8e8026f48cf177176700000160e073fe8f20ffffff0000000000000000000000000000000003b01098756bcf637bef2a161bd49412cad0a10adf12e64a694c41f9c5b971642029f0999def953f2a14ad6c143e2a0ebf3b4f794d8b17fc1203c12365427c09d3be653064be80f760b9d471dc9afbac2b24236c9f2eb0f08b7427942852dc780201000000000001022030759b07ca01caf8e524fc279946a1e96afc3546ee5f1fd4a1cfaf644763c2b4002c010000"
                |> Block.fromHex
                |> Option.get

            use client = ServiceBus.Client.create busName

            Messaging.Services.Blockchain.validateMinedBlock client block
        ()
#endif

module Init =
    
    let broker (config:MainConfig) =
        Actor.create (fun shim ->
            use poller = Poller.create ()
            use emObserver = Poller.registerEndMessage poller shim

            use sbBroker = ServiceBus.Broker.create poller busName config.serviceBusAddress
            use evBroker = EventBus.Broker.create poller busName config.publisherAddress

            Actor.signal shim
            Poller.run poller
        )
    
    let blockchain (config:MainConfig) =
        Blockchain.Main.main config.dataPath config.chainParams busName config.wipe
    
    let network (config:MainConfig) =
        let seeds =
            config.seeds
            |> Seq.choose (Endpoint.parseIp config.bind)
        
        Network.Main.main config.dataPath busName config.chainParams config.externalIp config.listen config.bind seeds config.wipe config.seed

        
    let api (config:MainConfig) =
        if config.api then
            Api.Main.main config.chain busName config.apiBind
            |> Disposables.toDisposable
        else
            Disposables.empty
    let wallet (config:MainConfig) =
        if config.wipeFull then Wallet.Main.Full elif config.wipe then Wallet.Main.Reset else Wallet.Main.NoWipe
        |> Wallet.Main.main config.dataPath busName config.chain config.wallet

    let addressDb (config:MainConfig) =
        if config.wipeFull then AddressDB.Main.Full elif config.wipe then AddressDB.Main.Reset else AddressDB.Main.NoWipe
        |> AddressDB.Main.main config.dataPath busName config.chain config.addressDb
    
    let miner (config:MainConfig) =
        if config.miner then
            Miner.Main.main busName config.minerThreads
            |> Disposables.toDisposable
        else
            Disposables.empty

        
module Config =
    type Config = YamlConfig<"scheme.yaml">

    let private getChain (config:Config) =
        match config.chain with
        | "main" -> Chain.Main
        | "test" -> Chain.Test
        | _ -> Chain.Local

    
    let get (results: ParseResults<Argument>): MainConfig =
        let config = Config()
        
        let mutable wipe = false
        let mutable wipeFull = false
        let mutable seed = false
        let mutable wallet = true
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
            | Test _ -> ()
#if DEBUG
            | Local idx ->
                chain <- Chain.Local
                config.dataPath <- sprintf "./data/local/%i" idx
                config.chain <- "local"
                config.externalIp <- Localhost
                config.bind <- getEndpoint Localhost (10000 + idx)
                config.api.bind <- getEndpoint Localhost (20000 + idx) 
                config.seeds.Clear ()

                if idx <> 0 then config.seeds.Add (getEndpoint Localhost 10000)
#endif
            | Api address ->
                config.api.enabled <- true
                config.api.bind <- address
            | Bind address ->
                config.bind <- address
                config.listen <- true
            | ConnectWallet ->
                config.api.enabled <- true
                config.api.bind <- getEndpoint LocalhostString (getPort config.api.bind)
                wallet <- false 
                addressDb <- true
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
        ) (results.GetAllResults())
        
        let chainParams = Chain.getChainParameters chain
        
        let dataPath = Platform.combine config.dataPath config.chain
        
        {
            chainParams       = chainParams
            wipe              = wipe
            wipeFull          = wipeFull
            seed              = seed
            wallet            = wallet
            addressDb         = addressDb
            miner             = config.miner.enabled
            minerThreads      = config.miner.threads
            api               = config.api.enabled
            serviceBusAddress = serviceBusAddress
            publisherAddress  = publisherAddress
            chain             = chain
            seeds             = config.seeds
            dataPath          = dataPath
            chainPath         = config.chain
            bind              = config.bind
            apiBind           = config.api.bind
            externalIp        = config.externalIp
            listen            = config.listen
        }
module CheckPlatform =
    
    let check =
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

[<EntryPoint>]
let main argv =

    CheckPlatform.check

    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)

    let parser = ArgumentParser.Create<Argument>(programName = "zen-node.exe", errorHandler = errorHandler)
    let results = parser.Parse argv

    let config = Config.get results
    
    eventX "DB is stored at: {dataPath}"
    >> setField "dataPath" (Platform.getAbsolutePath config.dataPath)
    |> Log.info
    
    use brokerActor     = Init.broker config
    
    use apiActor        = Init.api config
            
    use blockchainActor = Init.blockchain config

    use networkActor    = Init.network config

    use walletActor     = Init.wallet config

    use addressDbActor  = Init.addressDb config

    use minerActor      = Init.miner config
#if DEBUG        
    Local.addGenesis config
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