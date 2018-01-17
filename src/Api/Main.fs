module Api.Main

open Api
open FsNetMQ
open Infrastructure
open Messaging.Services.Network
open Messaging.Events
open FSharp.Control

type State = Server.T

let eventHandler event (state:State) = state

let main chain busName bind =
    Actor.create<unit,unit,Event,State> busName "Api" (fun poller sbObservable ebObservable ->            
        let server = Server.create chain poller busName bind
                
        let ebObservable = 
            ebObservable
            |> Observable.map eventHandler
            
        let httpObservable = 
            Server.observable server                        
            
        let observable =              
            ebObservable
            |> Observable.merge httpObservable             
            |> Observable.scan (fun state handler -> handler state) server        
    
        Disposables.toDisposable server, observable
    )