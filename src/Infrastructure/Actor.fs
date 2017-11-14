module Infrastructure.Actor

open FSharp.Control.Reactive
open Infrastructure

open FsNetMQ

type ActorFunction<'command,'request,'event,'result> = 
    Poller.T -> System.IObservable<ServiceBus.Agent.Message<'command, 'request>> -> System.IObservable<'event> -> System.IObservable<'result>

let create<'command,'request,'event,'result> busName serviceName (f:ActorFunction<'command,'request,'event,'result>) = 
    Actor.create (fun shim -> 
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim
            
        use sbAgent = ServiceBus.Agent.create<'command, 'request> poller busName serviceName
        use ebAgent = EventBus.Agent.create<'event> poller busName
        
        let observable = 
            f poller (ServiceBus.Agent.observable sbAgent) (EventBus.Agent.observable ebAgent)  
    
        let onError error = 
            Log.error "Unhandled exception %s %A" serviceName error
            System.Environment.FailFast(sprintf "Unhandled exception %s" serviceName, error)
    
        use observer = Observable.subscribeWithError ignore onError observable
        
        Actor.signal shim
        Poller.run poller
    )