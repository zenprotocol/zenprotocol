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
    Actor.create<Command, Request, Event, State> busName serviceName (fun poller sbObservable ebObservable ->           
        let peersManager = PeersManager.create poller listen bind seeds 
        
        let sbObservable = 
            sbObservable
            |> Observable.map (fun message ->
                match message with 
                | ServiceBus.Agent.Command c -> commandHandler c 
                | ServiceBus.Agent.Request (r, reply) -> requestHandler r reply)                
        
        let ebObservable = 
            ebObservable
            |> Observable.map eventHandler
           
        let observable =             
            Observable.merge sbObservable ebObservable
            |> Observable.merge (PeersManager.observable peersManager)
            |> Observable.scan (fun state handler -> handler state) peersManager             
    
        Disposables.toDisposable peersManager,observable
    )
                    