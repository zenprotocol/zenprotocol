module Network.Main

open Network
open FsNetMQ
open Infrastructure
open Messaging.Services.Network
open Messaging.Events
open FSharp.Control

type State = PeersManager.PeersManager

let eventHandler event (state:State) = state

let commandHandler command (state:State) = state

let requestHandler request reply (state:State) = state

let main busName externalIp listen bind seeds =
    Actor.create (fun shim ->   
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim
        
        use sbAgent = ServiceBus.Agent.create<Command, Request, Response> poller busName serviceName
        use ebAgent = EventBus.Agent.create<Event> poller busName
        use peersManager = PeersManager.create poller listen bind seeds 
        
        let sbObservable = 
            ServiceBus.Agent.observable sbAgent
            |> Observable.map (fun message ->
                match message with 
                | ServiceBus.Agent.Command c -> commandHandler c 
                | ServiceBus.Agent.Request (r, reply) -> requestHandler r reply)                
        
        let ebObservable = 
            EventBus.Agent.observable ebAgent
            |> Observable.map eventHandler
            
        use observer =             
            Observable.merge sbObservable ebObservable
            |> Observable.merge (PeersManager.observable peersManager)
            |> Observable.scan (fun state handler -> handler state) peersManager 
            |> Reactive.Observable.subscribe ignore     
        
        Actor.signal shim
        
        try 
            Poller.run poller
        with
        | x -> printfn "%A" x             
    )
                    