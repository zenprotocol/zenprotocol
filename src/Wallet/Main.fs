module Wallet.Main

open FsNetMQ
open Infrastructure
open Messaging.Services
open Messaging.Events
open Wallet
open ServiceBus.Agent

let eventHandler event account = 
    match event with 
    | TransactionAddedToMemPool (txHash,tx) ->
        Account.handleTransaction txHash tx account
    | BlockAdded block ->
        Account.handleBlock block account
    | _ -> account 

let commandHandler command wallet = wallet

let requestHandler chain client (requestId:RequestId) request wallet =
    let reply =
        requestId.reply
    let getTransactionResult =
        function
        | Result.Ok tx -> 
            TransactionResult.Ok tx
        | Result.Error err -> 
            TransactionResult.Error err

    match request with 
    | GetBalance -> 
        Account.getBalance wallet
        |> reply
    | GetAddress ->
        Account.getAddress chain wallet
        |> reply
    | Spend (address, spend) ->            
        Account.createTransaction chain wallet address spend
        |> getTransactionResult
        |> reply
    | ActivateContract code ->
        Account.createActivateContractTransaction wallet code
        |> function
        | Result.Ok tx -> 
            ActivateContractTransactionResult.Ok (tx, Consensus.Contract.computeHash code)
        | Result.Error err -> 
            ActivateContractTransactionResult.Error err
        |> reply
    | ExecuteContract (cHash, spends) ->   
        Account.createExecuteContractTransaction client chain cHash None spends
        |> getTransactionResult
        |> reply

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
                    