open System
open FsNetMQ
open FSharp.Configuration
open Argu
open Infrastructure
open Consensus
open Chain
open Logary.Message

module Actor = FsNetMQ.Actor

type Wipe =
    | Full

[<NoAppSettings>]
type Argument =
    | Chain of string
    | Api of string
    | Bind of string
    | Ip of string
    | Wipe of full:Wipe option
    | Miner of threads:int option
    | [<AltCommandLine("-lr");Hidden>] Localhost
    | [<AltCommandLine("-l1");Hidden>] Local1
    | [<AltCommandLine("-l2");Hidden>] Local2
    | [<Hidden>]Seed
    | Data_Path of string
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Api _ -> "Enable api and set bind address"
                | Bind _ -> "Set the address the node should listen on"
                | Chain _ -> "specify chain (local,test or main)."
                | Ip _ -> "specify the IP the node should relay to other peers"
                | Wipe _ -> "wipe database, specify full if you want wipe wallet private key"
                | Miner _ -> "enable miner and optionally specify number of threads"
                | Localhost -> "specify if the node should act as localhost, for tests only"
                | Local1 -> "run node with local1 settings, for tests only"
                | Local2 -> "run node with local2 settings, for tests only"
                | Seed -> "run node as a seed"
                | Data_Path _ -> "path to data folder"


type Config = YamlConfig<"scheme.yaml">

let busName = "main"

let createBroker () =
     Actor.create (fun shim ->
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim

        use sbBroker = ServiceBus.Broker.create poller busName
        use evBroker = EventBus.Broker.create poller busName

        Actor.signal shim
        Poller.run poller
)

let getChain (config:Config) =
    match config.chain with
    | "main" -> Main
    | "test" -> Test
    | _ -> Local

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)

    let config = new Config()
    config.Load("config.yaml")

    let parser = ArgumentParser.Create<Argument>(programName = "zen-node.exe", errorHandler = errorHandler)
    let results = parser.Parse argv

    let mutable wipe = false
    let mutable wipeFull = false

    use logary = Log.create

    eventX "Node running... press CTRL+C to exit"
    |> Log.info

    List.iter (fun arg ->
        match arg with
        | Chain chain -> config.chain <- chain
        | Localhost ->
            config.chain <- "local"
            config.externalIp <- "127.0.0.1"
            config.listen <- true
            config.seeds.Clear ()
            config.api.enabled <- true
        | Local1 ->
            config.dataPath <- "./data/l1"
            config.chain <- "local"
            config.listen <- true
            config.bind <- "127.0.0.1:37000"
            config.externalIp <- "127.0.0.1"
            config.api.enabled <- true
            config.api.bind <- "127.0.0.1:36000"
            config.seeds.Clear()
            config.seeds.Add "127.0.0.1:29555"
        | Local2 ->
            config.dataPath <- "./data/l2"
            config.chain <- "local"
            config.listen <- true
            config.bind <- "127.0.0.1:37001"
            config.externalIp <- "127.0.0.1"
            config.api.enabled <- true
            config.api.bind <- "127.0.0.1:36001"
            config.seeds.Clear()
            config.seeds.Add "127.0.0.1:29555"
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
        | Miner threads ->
            config.miner.enabled <- true

            match threads with
            | Some threads -> config.miner.threads <- threads
            | None -> ()
        | Seed ->
            config.listen <- true
            config.seeds.Clear ()
        | Data_Path dataPath ->
            config.dataPath <- dataPath
    ) (results.GetAllResults())

    let chain = getChain config
    let chainParams = getChainParameters chain
    let dataPath = Platform.combine config.dataPath config.chain

    use brokerActor = createBroker ()
    use blockchainActor = Blockchain.Main.main dataPath chainParams busName wipe

    use networkActor =
        Network.Main.main busName chainParams config.externalIp config.listen config.bind config.seeds

    use walletActor =
        if wipeFull then Wallet.Main.Full elif wipe then Wallet.Main.Reset else Wallet.Main.NoWipe
        |> Wallet.Main.main dataPath busName chain

    use minerActor =
        if config.miner.enabled then
            Miner.Main.main busName chainParams config.miner.threads
            |> Disposables.toDisposable
        else
            Disposables.empty

    use apiActor =
        if config.api.enabled then
            Api.Main.main chain busName config.api.bind
            |> Disposables.toDisposable
        else
            Disposables.empty

    if chain = Chain.Local then
        let (>>=) m f = Option.bind f m

//        let block = Consensus.Block.createGenesis Chain.localParameters [Consensus.Tests.Helper.rootTx] (0UL,0UL)
//        printfn "%A" (Block.hash block.header)
//        printfn "----"
//        printfn "%A" (Serialization.Block.serialize block |> FsBech32.Base16.encode)

        let block =
            "0000000000000000000000000000000000000000000000000000000000000000000000000000000197b2351c0f56e8d40f3ca37e344f06c00d75ce11c25b52141638be2c0761120a00000160e073fe8f20ffffff00000000000000000000000000000000030a3cecb5bae5b4eb8f6e18870674baa0987baf73c7bcf9a97d4c3456e00b638ced41d04d7002f9c0eb00f1b688fa5947b88ce1102eedfaeba1d4dff83c0694aabe653064be80f760b9d471dc9afbac2b24236c9f2eb0f08b7427942852dc780201000000000001012030759b07ca01caf8e524fc279946a1e96afc3546ee5f1fd4a1cfaf644763c2b4002c010000"
            |> FsBech32.Base16.decode
            >>= Serialization.Block.deserialize
            |> Option.get

        use client = ServiceBus.Client.create busName

        Messaging.Services.Blockchain.validateMinedBlock client block

    use event = new Threading.ManualResetEvent(false)

    Console.CancelKeyPress.Add (fun e ->
        e.Cancel <- true
        eventX "Closing..."
        |> Log.info
        event.Set() |> ignore
    )

    event.WaitOne() |> ignore

    0