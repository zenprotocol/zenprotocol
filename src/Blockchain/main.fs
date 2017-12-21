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
                | ServiceBus.Agent.Request (r, reply) -> Handler.handleRequest r reply) 
        let ebObservable = 
            ebObservable
            |> Observable.map Handler.handleEvent

        let utxoSet = 
            UtxoSet.create() 
            |> UtxoSet.handleTransaction ChainParameters.rootTxHash ChainParameters.rootTx
            
        let mempool = 
            MemPool.create ()
            |> MemPool.add ChainParameters.rootTxHash ChainParameters.rootTx            
                   
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