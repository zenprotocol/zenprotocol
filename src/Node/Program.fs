open FsNetMQ
open FSharp.Configuration
open Argu
open Infrastructure
open Consensus.Chain
open Consensus
open System

module Actor = FsNetMQ.Actor

[<NoAppSettings>]
type Argument =
    | Chain of string
    | Api of string
    | Bind of string
    | Ip of string
    | Wipe
    | Miner
    | [<AltCommandLine("-t")>] Threads of int
    | [<AltCommandLine("-lr")>] Localhost
    | [<AltCommandLine("-l1")>] Local1
    | [<AltCommandLine("-l2")>] Local2
    | Seed
    | Data_Path of string
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Api _ -> "Enable api and set bind address"
                | Bind _ -> "Set the address the node should listen on"
                | Chain _ -> "specify chain (local,test or main)."
                | Ip _ -> "specify the IP the node should relay to other peers"
                | Wipe -> "wipe database"
                | Miner -> "enable miner"
                | Threads _ -> "number of threads to use for miner"
                | Localhost -> "specify if the node should local chain host"
                | Local1 -> "run node with local1 settings, use for tests"
                | Local2 -> "run node with local1 settings, use for tests"
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
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some System.ConsoleColor.Red)

    let config = new Config()
    config.Load("config.yaml")

    let parser = ArgumentParser.Create<Argument>(programName = "zen-node.exe", errorHandler = errorHandler)
    let results = parser.Parse argv

    let mutable threads = 1
    let mutable wipe = false

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
        | Wipe -> wipe <- true
        | Miner ->
            config.miner <- true
        | Threads n ->
            config.threads <- n
        | Seed ->
            config.listen <- true
            config.seeds.Clear ()
            config.miner <- true
        | Data_Path dataPath ->
            config.dataPath <- dataPath
    ) (results.GetAllResults())

    let chain = getChain config
    let chainParams = getChainParameters chain
    let dataPath = Platform.combine config.dataPath config.chain

    if wipe then
        Log.info "wiping database"
        if System.IO.Directory.Exists dataPath then
                System.IO.Directory.Delete (dataPath,true)

    use brokerActor = createBroker ()
    use blockchainActor = Blockchain.Main.main dataPath chainParams busName

    use networkActor =
        Network.Main.main busName chainParams config.externalIp config.listen config.bind config.seeds

    use walletActor = Wallet.Main.main dataPath busName chain

    use minerActors =
        if config.miner then
            List.init config.threads ( fun _ ->
                Miner.Main.main busName chainParams
                |> Disposables.toDisposable )
            |> Disposables.fromList
        else
            [ Disposables.empty ]
            |> Disposables.fromList

    use apiActor =
        if config.api.enabled then
            Api.Main.main chain busName config.api.bind
            |> Disposables.toDisposable
        else
            Disposables.empty

    printfn "running..."

    if chain = Chain.Local then
        let (>>=) m f = Option.bind f m

        let block =
            "0000000000000000000000000000000000000000000000000000000000000000000000000000000" +
            "10ee6ef1eb8bcf752290c0765c52e83a0cf72963773c3aa6f18524f97b4b55ee100000160e073fe" +
            "8f20ffffff000000000000000000000000000000000000000327568a196fd2af61b99bd5578e80f" +
            "44bb4b685973fd87f334623a503b5b67c65f54f0947eb311b6fdd36ccd5ab7b8fada7b55502abba" +
            "816e8d65d83a26092ed9be653064be80f760b9d471dc9afbac2b24236c9f2eb0f08b7427942852d" +
            "c780200000001000000000000000101eca101ba1e938c6a8cd10e031f2ac363f4176dcf4450e244" +
            "2ebedb825fd33b1e000000000000000000000000000000000000000000000000000000000000000" +
            "000000000000000000000000000000000000000000000000000000000000000000000000005f5e1" +
            "000000000000"
            |> FsBech32.Base16.decode
            >>= Serialization.Block.deserialize
            |> Option.get

        use client = ServiceBus.Client.create busName

        Messaging.Services.Blockchain.validateBlock client block

    printfn "Press CTRL+C to exit"

    use event = new System.Threading.ManualResetEvent(false)

    System.Console.CancelKeyPress.Add (fun e ->
        e.Cancel <- true
        printfn "Close requested"
        event.Set() |> ignore
    )

    event.WaitOne() |> ignore

    printfn "Closing..."

    0 // return an integer exit code