module Blockchain.Main

open FsNetMQ
open Infrastructure
open Messaging.Services.Blockchain
open Messaging.Events

// TODO: should be the state of the blockchain actor
type State = unit     

let eventHandler event (state:State) = state

let commandHandler command (state:State) = state

let requestHandler request reply (state:State) = state

let main busName =
    Actor.create busName serviceName (fun poller sbObservable ebObservable  ->                   
        let sbObservable = 
            sbObservable
            |> Observable.map (fun message ->
                match message with 
                | ServiceBus.Agent.Command c -> commandHandler c 
                | ServiceBus.Agent.Request (r, reply) -> requestHandler r reply)                
        
        let ebObservable = 
            ebObservable
            |> Observable.map eventHandler
            
                     
        Observable.merge sbObservable ebObservable
        |> Observable.scan (fun state handler -> handler state) ()                 
    )
                    