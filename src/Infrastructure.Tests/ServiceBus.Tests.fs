module Infrastructure.ServiceBus.Tests

open NUnit.Framework
open FsUnit
open FsNetMQ
open Infrastructure.ServiceBus

type Commands =
    | Greet

type Request =
    | Hello

type Response =
    | World

let busName = "test"

let createBroker () =
     Actor.create (fun shim ->
        use poller = Poller.create ()
        use broker = Broker.create poller busName None

        use observer = Poller.registerEndMessage poller shim

        Actor.signal shim

        Poller.run poller

        Poller.removeSocket poller shim)

let createAgent delay =
     Actor.create (fun shim ->
        if delay > 0 then
            Actor.signal shim
            System.Threading.Thread.Sleep delay

        use poller = Poller.create ()
        use agent = Agent.create<Commands, Request> poller busName "greeter"

        use observer = Poller.registerEndMessage poller shim

        use agentObserver =
            Agent.observable agent
            |> Observable.subscribe (fun msg ->
                match msg with
                | Agent.Request (requestId, _) -> requestId.reply World
                | _ -> ())

        if delay = 0 then Actor.signal shim

        Poller.run poller

        Poller.removeSocket poller shim)

let sendCommand delay service command =
    Actor.create (fun shim ->
        Actor.signal shim

        if delay > 0 then System.Threading.Thread.Sleep delay

        use client = Client.create "test"

        Client.Command.send client service command)

[<Test>]
let ``send command while agent is up`` () =

    use broker = createBroker ()
    use client = sendCommand 10 "greeter" Greet

    use poller = Poller.create ()
    use agent = Agent.create<Commands, Request> poller "test" "greeter"

    use observer =
        Agent.observable agent
        |> Observable.subscribe (fun msg ->

            Poller.stop poller
            match msg with
            | Agent.Request _ -> failwith "Expect Command"
            | Agent.Command c ->
                c |> should equal Greet)

    Poller.run poller

[<Test>]
let ``send command while agent still not up`` () =

    use broker = createBroker ()
    use client = sendCommand 0 "greeter" Greet

    System.Threading.Thread.Sleep 10

    use poller = Poller.create ()
    use agent = Agent.create<Commands, Request> poller "test" "greeter"

    use observer =
        Agent.observable agent
        |> Observable.subscribe (fun msg ->

            Poller.stop poller
            match msg with
            | Agent.Request _ -> failwith "Expect Command"
            | Agent.Command c ->
                c |> should equal Greet)

    Poller.run poller

[<Test>]
let ``send request while agent is up`` () =

    use broker = createBroker ()
    use agent = createAgent 0
    use client = Client.create busName

    System.Threading.Thread.Sleep 10

    let response = Client.Request.send<Request, Response> client "greeter" Hello

    response |> should equal (World)

[<Test>]
let ``send request while agent is still not up`` () =

    use broker = createBroker ()
    use agent = createAgent 10
    use client = Client.create busName

    let response = Client.Request.send<Request, Response> client "greeter" Hello

    response |> should equal (World)
