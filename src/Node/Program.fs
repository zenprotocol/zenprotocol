open FsNetMQ
open FSharp.Configuration
open Argu
open Infrastructure
open Consensus.ChainParameters

module Actor = FsNetMQ.Actor

[<NoAppSettings>]
type Argument = 
    | Chain of string
    | Localhost    
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Chain _ -> "specify chain (local,test or main)."
                | Localhost -> "specify if the node should local chain host"                              

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

let getNetworkParameters (config:Config) = 
    match config.chain with
    | "local" -> config.networks.local.listen,config.networks.local.bind,config.networks.local.seeds
    | "test" -> config.networks.test.listen,config.networks.test.bind,config.networks.test.seeds
    | "main" -> config.networks.main.listen,config.networks.main.bind,config.networks.main.seeds
    | c -> failwithf "unkown chain %s" c 

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
            config.networks.local.listen <- true
            config.networks.local.seeds.Clear ()  
            root <- true            
    ) (results.GetAllResults())
                              
    use brokerActor = createBroker ()    
    use blockchainActor = Blockchain.Main.main busName
    
    let listen,bind,seeds = getNetworkParameters config
    use networkActor = Network.Main.main busName config.externalIp listen bind seeds
    
    let chain = getChain config
    use walletActor = Wallet.Main.main busName chain root
    
    use apiActor =    
        if config.api.enabled then 
            (Api.Main.main busName config.api.bind) :> System.IDisposable
        else            
            { new System.IDisposable with member x.Dispose() = ()}                            
                       
    printfn "running..."
    
    printfn "Press enter to exit"
    
    System.Console.ReadLine () |> ignore        
    
    0 // return an integer exit code