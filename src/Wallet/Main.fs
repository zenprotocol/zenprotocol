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
        Account.addTransaction txHash tx account
    | BlockAdded (blockHash,block) ->
        Account.handleBlock blockHash block account
    | BlockRemoved (_,block) ->
        Account.undoBlock block account            
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
    | ExecuteContract (cHash,command, spends) ->   
        let executeContract = Blockchain.executeContract client 
        Account.createExecuteContractTransaction wallet executeContract cHash command spends
        |> getTransactionResult
        |> reply

    wallet

let main busName chain root =
    Actor.create<Command,Request,Event, Account.T> busName serviceName (fun poller sbObservable ebObservable ->                                                             
        let client = ServiceBus.Client.create busName                 
        let account = 
            let account = if root then Account.createRoot () else Account.create ()
            match Blockchain.getTip client with
            | Some (blockHash,header) ->
                                     
                let account =
                    Account.sync chain blockHash                    
                        (Blockchain.getBlockHeader client >> Option.get)
                        (Blockchain.getBlock client >> Option.get)
                        account
                    
                Log.info "Account synced to block #%d %A" header.blockNumber blockHash
                account                      
            | _ -> account               

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
            |> Observable.scan (fun state handler -> handler state) account             
    
        Disposables.empty, observable
    )
                    