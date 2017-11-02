module Blockchain.Main

open FsNetMQ
open Infrastructure
open Messaging.Services.Blockchain
open Messaging.Events

// TODO: should be the state of the blockchain actor
type State = unit     

let eventHandler event (state:State) = state

let commandHandler command (state:State) = state

let requestHandler request reply (state:State) = state

let main busName =
    Actor.create (fun shim ->   
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim
        
        use sbAgent = ServiceBus.Agent.create<Command, Request, Response> poller busName serviceName
        use ebAgent = EventBus.Agent.create<Event> poller busName
        
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
            |> Observable.scan (fun state handler -> handler state) () 
            |> Observable.subscribe ignore     
        
        Actor.signal shim
        Poller.run poller
    )
                    