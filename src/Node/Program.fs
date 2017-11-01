open FsNetMQ
open Infrastructure

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

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    
    use brokerActor = createBroker ()    
    use blockchainActor = Blockchain.Main.main busName
    
    printfn "running..."
    
    System.Console.ReadLine () |> ignore
    
    0 // return an integer exit code
