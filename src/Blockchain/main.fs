module Blockchain.Main

open FsNetMQ
open FSharp.Control
open Infrastructure
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus

type State = UtxoSet.T * MemPool.T * OrphanPool.T * ActiveContractSet.T

let main busName =
    Actor.create<Command,Request,Event,State> busName serviceName (fun poller sbObservable ebObservable  ->  
        let publisher = EventBus.Publisher.create<Event> busName
        let client = ServiceBus.Client.create busName
                                
        let sbObservable = 
            sbObservable
            |> Observable.map (fun message ->                
                match message with 
                | ServiceBus.Agent.Command c -> Handler.handleCommand c 
                | ServiceBus.Agent.Request (requestId, request) -> 
                    Handler.handleRequest requestId.reply request)                
        
        let ebObservable = 
            ebObservable
            |> Observable.map Handler.handleEvent

        let utxoSet = 
            UtxoSet.create() 
            |> UtxoSet.handleTransaction Transaction.rootTxHash Transaction.rootTx
            
        let mempool = 
            MemPool.create ()
            |> MemPool.add Transaction.rootTxHash Transaction.rootTx            
                   
        let orphanPool = 
            OrphanPool.create ()                   

        let acs = ActiveContractSet.create ()
                     
        let observable =                      
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun state handler -> 
                let effectWriter = handler state
                EffectsWriter.run effectWriter publisher client
                ) (utxoSet,mempool,orphanPool,acs)             
    
        Disposables.empty, observable 
    )