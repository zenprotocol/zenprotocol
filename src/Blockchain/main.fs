module Blockchain.Main

open FsNetMQ
open FSharp.Control
open Infrastructure
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus
open State

let main chain busName =
    Actor.create<Command,Request,Event,State> busName serviceName (fun poller sbObservable ebObservable  ->  
        let publisher = EventBus.Publisher.create<Event> busName
        let client = ServiceBus.Client.create busName
                                        
        let sbObservable = 
            sbObservable
            |> Observable.map (fun message ->                
                match message with 
                | ServiceBus.Agent.Command c -> Handler.handleCommand chain c 
                | ServiceBus.Agent.Request (requestId, request) -> 
                    Handler.handleRequest requestId.reply request)                
        
        let ebObservable = 
            ebObservable
            |> Observable.map Handler.handleEvent

        let tipState = 
            {
                activeContractSet=ActiveContractSet.empty;
                utxoSet=UtxoSet.create()
                ema=EMA.create chain 
                tip=PersistentBlock.empty
            }
            
        let memoryState = 
            {
                activeContractSet=ActiveContractSet.empty;
                utxoSet=UtxoSet.create()
                mempool=MemPool.empty
                orphanPool=OrphanPool.create ()
            }
            
        let state = 
            {
                memoryState = memoryState;
                tipState = tipState;
                blockRepository = BlockRepository.create ()
                blockRequests= Set.empty
            }            
                     
        let observable =                      
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun state handler -> 
                let effectWriter = handler state
                EffectsWriter.run effectWriter publisher client
                ) state  
                                                                                      
        Disposables.empty, observable 
    )