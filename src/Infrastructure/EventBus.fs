module Infrastructure.EventBus

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

    let create poller name publisherAddress =
        let publisher = Socket.pub ()
        Options.setSendHighWatermark publisher 0 |> ignore
        Socket.bind publisher (getPublisherAddress name)
        Option.iter (fun publisherAddress -> Socket.bind publisher publisherAddress) publisherAddress

        let subscriber = Socket.sub ()
        Options.setRecvHighwatermark subscriber 0 |> ignore
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

module Subscriber =
    type Subscriber<'event> =
        | Subscriber of Socket.T
        interface System.IDisposable with
            member x.Dispose () =
                let (Subscriber subscriber) = x
                (subscriber :> System.IDisposable).Dispose ()

    let create<'event> name : Subscriber<'event> =
        let subscriber = Socket.sub ()
        Options.setRecvHighwatermark subscriber 0 |> ignore

        Socket.subscribe subscriber ""
        Socket.connect subscriber (getPublisherAddress name)

        Subscriber subscriber

    let createByAddress<'event> address : Subscriber<'event> =
        let subscriber = Socket.sub ()
        Options.setRecvHighwatermark subscriber 0 |> ignore

        Socket.subscribe subscriber ""
        Socket.connect subscriber address

        Subscriber subscriber

    let recv<'event> (Subscriber subscriber:Subscriber<'event>) : 'event =
        SingleFrame.recv subscriber
        |> binarySerializer.UnPickle<'event>

    let tryRecv<'event> (Subscriber subscriber:Subscriber<'event>) timeout : 'event option =
        SingleFrame.tryRecv subscriber timeout
        |> Option.map (binarySerializer.UnPickle<'event>)

module Agent =
    type Agent<'a> = {
        subscriber: Socket.T;
        poller: Poller.T;
        observable: System.IObservable<'a>;
        observer: System.IDisposable;
    }

    type T<'a> =
        | Agent of Agent<'a>
        interface System.IDisposable with
            member x.Dispose () =
                match x with
                | Agent agent ->
                    Poller.removeSocket agent.poller agent.subscriber
                    agent.observer.Dispose()
                    (agent.subscriber :> System.IDisposable).Dispose ()

    let create<'a> poller name =

        let subscriber = Socket.sub ()
        Options.setRecvHighwatermark subscriber 0 |> ignore
        Socket.subscribe subscriber ""
        Socket.connect subscriber (getPublisherAddress name)

        let observable =
            Poller.addSocket poller subscriber
            |> Observable.map (fun _ ->
                SingleFrame.recv subscriber
                |> binarySerializer.UnPickle<'a>
            )
            |> FSharp.Control.Reactive.Observable.publish

        let observer = FSharp.Control.Reactive.Observable.connect observable

        let agent = {
            subscriber=subscriber;
            poller = poller;
            observable = observable;
            observer = observer;
        }

        Agent agent

    let observable (Agent agent) = agent.observable

module Publisher =
    type T<'a> =
        | Publisher of Socket.T
        interface System.IDisposable with
            member x.Dispose () =
                match x with
                | Publisher socket ->
                    (socket :> System.IDisposable).Dispose ()

    let create<'a> name : T<'a> =
         let publisher = Socket.pub ()
         Options.setSendHighWatermark publisher 0 |> ignore
         Socket.connect publisher (getSubscriberAddress name)

         Publisher publisher

    let publish :(T<'a> -> 'a -> unit) = fun (Publisher socket) msg ->
        binarySerializer.Pickle<'a> msg
        |> Frame.send socket
