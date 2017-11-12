module Api.Main

open Api
open FsNetMQ
open Infrastructure
open Messaging.Services.Network
open Messaging.Events
open FSharp.Control

type State = Server.T

let eventHandler event (state:State) = state

let main busName bind =
    Actor.create (fun shim ->   
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim                     
                
        use ebAgent = EventBus.Agent.create<Event> poller busName
        use server = Server.create poller bind
                
        let ebObservable = 
            EventBus.Agent.observable ebAgent
            |> Observable.map eventHandler
            
        let httpObservable = 
            Server.observable server                        
            
        use observer =             
            ebObservable
            |> Observable.merge httpObservable             
            |> Observable.scan (fun state handler -> handler state) server
            |> Reactive.Observable.subscribeWithError ignore (fun error ->            
                Log.error "Unhandled exception %A" error
                System.Environment.FailFast (sprintf "Unhandled exception %A" error)            
            )
        
        Actor.signal shim
        Poller.run poller
    )