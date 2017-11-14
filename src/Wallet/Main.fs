module Wallet.Main

open FsNetMQ
open Infrastructure
open Messaging.Services.Wallet
open Messaging.Events
open Wallet

let eventHandler event wallet = wallet

let commandHandler command wallet = wallet

let requestHandler agent requestId request wallet = 
    match request with 
    | GetBalance -> 
        let balance = Wallet.getBalance wallet
        ServiceBus.Agent.reply agent requestId balance
        wallet
    | GetAddress ->
        let address = Wallet.getAddress wallet
        ServiceBus.Agent.reply agent requestId address
        wallet
    | _ -> wallet

let main busName =
    Actor.create (fun shim ->               
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim
        
        use sbAgent = ServiceBus.Agent.create<Command, Request> poller busName serviceName
        use ebAgent = EventBus.Agent.create<Event> poller busName
        
        let wallet = Wallet.create ()
        
        let sbObservable = 
            ServiceBus.Agent.observable sbAgent
            |> Observable.map (fun message ->
                match message with 
                | ServiceBus.Agent.Command c -> commandHandler c 
                | ServiceBus.Agent.Request (requestId, r) -> requestHandler sbAgent requestId r)                
        
        let ebObservable = 
            EventBus.Agent.observable ebAgent
            |> Observable.map eventHandler
            
        use observer =             
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun state handler -> handler state) wallet  
            |> Observable.subscribe ignore     
        
        Actor.signal shim
        Poller.run poller
    )
                    