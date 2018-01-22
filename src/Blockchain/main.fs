module Blockchain.Main

open FsNetMQ
open FSharp.Control
open Infrastructure
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus
open State

let main dataPath chain busName =
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

        let databaseContext = DatabaseContext.create dataPath                              

        let tip,utxoSet,acs,ema = 
            use session = DatabaseContext.createSession databaseContext
            match BlockRepository.tryGetTip session with 
            | Some (tip,utxoSet,acs,ema) ->
                Log.info "Loading tip from db #%d %A" tip.header.blockNumber tip.hash
                         
                tip,utxoSet,acs,ema
            | None -> 
                Log.info "No tip in db"
                ExtendedBlockHeader.empty,UtxoSet.create(),ActiveContractSet.empty,EMA.create chain

        let tipState = 
            {
                activeContractSet=acs
                utxoSet=utxoSet
                ema=ema
                tip=tip
            }
            
        let memoryState = 
            {
                activeContractSet=acs
                utxoSet=utxoSet
                mempool=MemPool.empty
                orphanPool=OrphanPool.create ()
            }
            
        let state = 
            {
                memoryState = memoryState;
                tipState = tipState;
                blockRequests= Map.empty
            }                                
                     
        let observable =                      
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun state handler -> 
                use session = DatabaseContext.createSession databaseContext
                let effectWriter = handler session (Timestamp.now ()) state
                let result = EffectsWriter.run effectWriter publisher client                
                DataAccess.Session.commit session.session
                
                result 
                ) state  
                                                          
        Disposables.toDisposable databaseContext, observable 
    )