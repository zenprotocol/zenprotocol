open FsNetMQ
open FSharp.Configuration
open Argu
open Infrastructure
open Consensus.ChainParameters
open Consensus

module Actor = FsNetMQ.Actor

[<NoAppSettings>]
type Argument = 
    | Chain of string
    | Api of string
    | Bind of string
    | Ip of string
    | [<AltCommandLine("-lr")>] Localhost
    | [<AltCommandLine("-l1")>] Local1
    | [<AltCommandLine("-l2")>] Local2
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Api _ -> "Enable api and set bind address"
                | Bind _ -> "Set the address the node should listen on"
                | Chain _ -> "specify chain (local,test or main)."                
                | Ip _ -> "specify the IP the node should relay to other peers"
                | Localhost -> "specify if the node should local chain host"
                | Local1 -> "run node with local1 settings, use for tests"
                | Local2 -> "run node with local1 settings, use for tests"
                                              

type Config = YamlConfig<"config.yaml">

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
    | _ -> Test

[<EntryPoint>]
let main argv = 
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some System.ConsoleColor.Red)

    let config = new Config()           
    config.Load("config.yaml")        
    
    let parser = ArgumentParser.Create<Argument>(programName = "zen-node.exe", errorHandler = errorHandler)            
    let results = parser.Parse argv
    
    let mutable root = false 
    
    List.iter (fun arg -> 
        match arg with 
        | Chain chain -> config.chain <- chain            
        | Localhost -> 
            config.chain <- "local"
            config.externalIp <- "127.0.0.1"
            config.listen <- true
            config.seeds.Clear ()  
            config.api.enabled <- true
            config.miner <- true
            root <- true 
        | Local1 ->
            config.chain <- "local"
            config.listen <- true
            config.bind <- "127.0.0.1:37000"
            config.externalIp <- "127.0.0.1"
            config.api.enabled <- true
            config.api.bind <- "127.0.0.1:36000"
        | Local2 ->
            config.chain <- "local"
            config.listen <- true
            config.bind <- "127.0.0.1:37001"
            config.externalIp <- "127.0.0.1"
            config.api.enabled <- true
            config.api.bind <- "127.0.0.1:36001"
        | Api address -> 
            config.api.enabled <- true
            config.api.bind <- address
        | Bind address ->
            config.bind <- address
            config.listen <- true
        | Ip ip ->
            config.externalIp <- ip                
                                       
    ) (results.GetAllResults())
    
    let chain = getChain config
    use brokerActor = createBroker ()    
    use blockchainActor = Blockchain.Main.main chain busName
        
    use networkActor = 
        Network.Main.main busName config.externalIp config.listen config.bind config.seeds
        
    use walletActor = Wallet.Main.main busName chain root
    
    use minerActor =
        if config.miner then        
            Miner.Main.main busName chain
            |> Disposables.toDisposable
        else
            Disposables.empty 
    
    use apiActor =    
        if config.api.enabled then 
            Api.Main.main chain busName config.api.bind
            |> Disposables.toDisposable
        else            
            Disposables.empty                            
                       
    printfn "running..."
       
    if root then    
        let block = Block.createGenesis chain [Transaction.rootTx] (0UL,0UL)
        
        use client = ServiceBus.Client.create busName  
        
        Messaging.Services.Blockchain.validateBlock client block
                
    printfn "Press enter to exit"
    
    System.Console.ReadLine () |> ignore
    
    0 // return an integer exit code