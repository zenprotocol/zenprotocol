module Network.Main

open FSharp.Control
open Network
open FsNetMQ
open Infrastructure
open Messaging.Services.Network
open Messaging.Events
open Consensus
open Messaging

type State = PeersManager.PeersManager

let eventHandler event (peersManager:State) = 
    match event with 
    | Event.TransactionAddedToMemPool (txHash, tx) ->
        let bytes = Transaction.serialize Transaction.Full tx
        let msg = Message.Transaction {tx=bytes}
            
        PeersManager.publish msg peersManager
    | _ -> peersManager

let networkHandler client msg (state:State) = 
    match msg with 
    | Message.Transaction msg ->
        match Transaction.deserialize msg.tx with
        | Some tx ->
            Services.Blockchain.validateTransaction client tx
            state
        | None ->
            //TODO: log non-deserializable transaction
            state
    | _ -> 
        // TODO: log unknown message
        state

let commandHandler command (state:State) = state

let requestHandler request reply (state:State) = state

let main busName externalIp listen bind seeds =
    Actor.create<Command, Request, Event, State> busName serviceName (fun poller sbObservable ebObservable ->           
        let peersManager = PeersManager.create poller listen bind seeds 
        
        let client = ServiceBus.Client.create busName
        
        let sbObservable = 
            sbObservable
            |> Observable.map (fun message ->
                match message with 
                | ServiceBus.Agent.Command c -> commandHandler c 
                | ServiceBus.Agent.Request (r, reply) -> requestHandler r reply)                
        
        let ebObservable = 
            ebObservable
            |> Observable.map eventHandler
            
        let networkObservable = 
            PeersManager.messageObservable peersManager
            |> Observable.map (networkHandler client)
           
        let observable =             
            Observable.merge sbObservable ebObservable
            |> Observable.merge networkObservable
            |> Observable.merge (PeersManager.observable peersManager)
            |> Observable.scan (fun state handler -> handler state) peersManager             
    
        Disposables.toDisposable peersManager,observable
    )
                    
