module Wallet.Main

open FsNetMQ
open Infrastructure
open Messaging.Services.Wallet
open Messaging.Events
open Wallet

let eventHandler event account = 
    match event with 
    | TransactionAddedToMemPool (txHash,tx) ->
        Account.handleTransaction txHash tx account
    | BlockAdded block ->
        Account.handleBlock block account
    | _ -> account 

let commandHandler command wallet = wallet

let requestHandler chain client (requestId:ServiceBus.Agent.RequestId) request wallet = 
    match request with 
    | GetBalance -> 
        let balance = Account.getBalance wallet
        requestId.reply balance
        wallet
    | GetAddress ->
        let address = Account.getAddress wallet chain
        requestId.reply address
        wallet
    | CreateTransaction (address, asset, amount) ->            
        match Account.createTransaction chain wallet address asset amount with
        | Ok tx -> 
            requestId.reply (Created tx)
            
        | Result.Error tx -> 
            requestId.reply (Error tx)
        
        wallet
    | CreateContractActivationTransaction (code) ->
        match Account.createContractActivationTransaction wallet code with
        | Ok tx -> 
            requestId.reply (Created tx)
            
        | Result.Error tx -> 
            requestId.reply (Error tx)
        
        wallet
    | CreateSendMessageTranscation (address, asset, amount) ->            
        match Account.createSendMessageTranscation client chain address with
        | Ok tx -> 
            requestId.reply (Created tx)
            
        | Result.Error tx -> 
            requestId.reply (Error tx)
        
        wallet
    | _ ->
        wallet

let main busName chain root =
    Actor.create<Command,Request,Event, Account.T> busName serviceName (fun poller sbObservable ebObservable ->                       
        let wallet = if root then Account.createRoot () else Account.create ()
        let client = ServiceBus.Client.create busName

        let sbObservable = 
            sbObservable
            |> Observable.map (fun message ->
                match message with 
                | ServiceBus.Agent.Command c -> commandHandler c 
                | ServiceBus.Agent.Request (requestId, r) -> requestHandler chain client requestId r)                
        
        let ebObservable = 
            ebObservable
            |> Observable.map eventHandler
           
        let observable =             
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun state handler -> handler state) wallet             
    
        Disposables.empty, observable
    )
                    