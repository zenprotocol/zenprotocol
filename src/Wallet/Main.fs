module Wallet.Main

open FsNetMQ
open Infrastructure
open Messaging.Services.Wallet
open Messaging.Events
open Wallet

let eventHandler event wallet = wallet

let commandHandler command wallet = wallet

let requestHandler (requestId:ServiceBus.Agent.RequestId) request wallet = 
    match request with 
    | GetBalance -> 
        let balance = Account.getBalance wallet
        requestId.reply balance
        wallet
    | GetAddress ->
        let address = Account.getAddress wallet
        requestId.reply address
        wallet
    | _ -> wallet

let main busName =
    Actor.create busName serviceName (fun poller sbObservable ebObservable ->                       
        let wallet = Account.create ()
        
        let sbObservable = 
            sbObservable
            |> Observable.map (fun message ->
                match message with 
                | ServiceBus.Agent.Command c -> commandHandler c 
                | ServiceBus.Agent.Request (requestId, r) -> requestHandler requestId r)                
        
        let ebObservable = 
            ebObservable
            |> Observable.map eventHandler
           
        let observable =             
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun state handler -> handler state) wallet             
    
        Disposables.empty, observable
    )
                    