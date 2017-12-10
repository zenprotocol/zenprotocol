module Blockchain.Main

open FsNetMQ
open FSharp.Control
open Infrastructure
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus

// TODO: should be the state of the blockchain actor
type State = UtxoSet.T * MemPool.T

let eventHandler event (state:State) = state

let commandHandler publisher command (utxoSet, mempool) = 
    match command with
    | ValidateTransaction tx ->
        match Transaction.validate utxoSet tx with
        | Ok tx ->      
            let txHash = Transaction.hash tx
            
            match MemPool.containsTransaction txHash mempool with
            | true -> utxoSet,mempool // Nothing to do, already in mempool
            | false ->                             
                let utxoSet = UtxoSet.handleTransaction txHash tx utxoSet
                let mempool = MemPool.add txHash tx mempool
                
                EventBus.Publisher.publish publisher (TransactionAddedToMemPool (txHash,tx))
                
                Log.info "Transaction %s added to mempool" (Hash.toString txHash)
                
                utxoSet,mempool
        | Error error -> 
            // TODO: we should do something with the error here, like writing to log
            // and banning peer?
            // TODO: check if should be added to orphan list
            
            utxoSet,mempool
                         
    | _ -> utxoSet,mempool

let requestHandler request reply (state:State) = state

let main busName =
    Actor.create<Command,Request,Event,State> busName serviceName (fun poller sbObservable ebObservable  ->  
        let publisher = EventBus.Publisher.create<Event> busName
                                
        let sbObservable = 
            sbObservable
            |> Observable.map (fun message ->
                match message with 
                | ServiceBus.Agent.Command c -> commandHandler publisher c 
                | ServiceBus.Agent.Request (r, reply) -> requestHandler r reply)                
        
        let ebObservable = 
            ebObservable
            |> Observable.map eventHandler

        let utxoSet = 
            UtxoSet.create() 
            |> UtxoSet.handleTransaction ChainParameters.rootTxHash ChainParameters.rootTx
            
        let mempool = 
            MemPool.create ()
            |> MemPool.add ChainParameters.rootTxHash ChainParameters.rootTx            
                     
        let observable =                      
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun state handler -> handler state) (utxoSet,mempool)             
    
        Disposables.empty, observable 
    )
                    