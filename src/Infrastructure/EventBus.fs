module EventBus

open FsNetMQ
open MBrace.FsPickler

let binarySerializer = FsPickler.CreateBinarySerializer()

let private getPublisherAddress name = 
    sprintf "inproc://%s-pub" name
    
let private getSubscriberAddress name =
    sprintf "inproc://%s-sub" name     

module Broker =
    type Broker = {
        subscriber: Socket.T;
        publisher: Socket.T; 
        poller: Poller.T;
        observer: System.IDisposable   
    }
 
    type T = 
        | Broker of Broker
        interface System.IDisposable with 
            member x.Dispose () =
                match x with 
                | Broker broker ->
                    broker.observer.Dispose ()
                    Poller.removeSocket broker.poller broker.subscriber                            
                    (broker.publisher :> System.IDisposable).Dispose ()
                    (broker.subscriber :> System.IDisposable).Dispose ()   
    
    let create poller name =
        // TODO: unlock highwatermark
                 
        let publisher = Socket.pub ()
        Socket.bind publisher (getPublisherAddress name)
        
        let subscriber = Socket.sub ()
        Socket.bind subscriber (getSubscriberAddress name)
        Socket.subscribe subscriber ""
        
        let observer =
            Poller.addSocket poller subscriber
            |> Observable.subscribe (fun _ ->                
                let msg = Multipart.recv subscriber
                Multipart.send publisher msg)
        
        Broker {
            publisher=publisher;
            subscriber=subscriber;
            observer=observer;
            poller=poller;
        }
            
module Agent =    
    type Agent<'a> = {
        subscriber: Socket.T;
        publisher: Socket.T; 
        poller: Poller.T;   
        observable: System.IObservable<'a>;    
    }  

    type T<'a> = 
        | Agent of Agent<'a>
        interface System.IDisposable with 
            member x.Dispose () =
                match x with 
                | Agent agent ->
                    Poller.removeSocket agent.poller agent.subscriber 
                    (agent.publisher :> System.IDisposable).Dispose ()
                    (agent.subscriber :> System.IDisposable).Dispose () 

    let create<'a> poller name =
        let publisher = Socket.pub ()
        Socket.connect publisher (getSubscriberAddress name)
        
        let subscriber = Socket.sub ()
        Socket.connect subscriber (getPublisherAddress name)
        Socket.subscribe subscriber ""
        
        let observable = 
            Poller.addSocket poller subscriber
            |> Observable.map (fun _ ->                                                
                SingleFrame.recv subscriber
                |> binarySerializer.UnPickle<'a>
            )
            
        let agent = {
            publisher=publisher;
            subscriber=subscriber;
            poller = poller;
            observable = observable;
        }
        
        Agent agent                    

    let observable (Agent agent) = agent.observable        
        
    let publish:(T<'a> -> 'a -> unit) = fun (Agent agent) msg -> 
        binarySerializer.Pickle msg
        |> Frame.send agent.publisher                      
        
                                                            

