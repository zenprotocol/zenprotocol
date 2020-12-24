module Api.Tests.Utils

open System
open FsNetMQ
open FSharp.Data
open FsUnit
open Consensus
open Infrastructure

module Actor = FsNetMQ.Actor
[<Literal>]
let private RunActor = true
let busName = "test"
let chain = Chain.Local

let chainParams = Chain.getChainParameters chain
let tempDir () =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]
let dataPath = tempDir()
let apiUri = "127.0.0.1:29555"

let createBroker () =
     Actor.create (fun shim ->
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim

        use sbBroker = ServiceBus.Broker.create poller busName None
        use evBroker = EventBus.Broker.create poller busName None

        Actor.signal shim
        Poller.run poller
    )
let getTestnetActors () =
    [
        createBroker ()
        Blockchain.Main.main dataPath (Chain.getChainParameters Chain.Test) busName false
        Network.Main.main dataPath busName (Chain.getChainParameters Chain.Test) "" false "" [] false false
        Wallet.Main.main dataPath busName Chain.Test RunActor Wallet.Main.Wipe.NoWipe
        Api.Main.main Chain.Test busName apiUri
    ]
    |> List.rev
    |> List.map Disposables.toDisposable
    |> Disposables.fromList


let getActors () =
    [
        createBroker ()
        Blockchain.Main.main dataPath chainParams busName false
        Network.Main.main dataPath busName chainParams "" false "" [] false false
        Wallet.Main.main dataPath busName chain RunActor Wallet.Main.Wipe.NoWipe
        AddressDB.Main.main dataPath busName chain RunActor AddressDB.Main.NoWipe
        Api.Main.main chain busName apiUri
    ]
    |> List.rev
    |> List.map Disposables.toDisposable
    |> Disposables.fromList


let getBasicActors () =
    [
        createBroker ()
        Blockchain.Main.main dataPath chainParams busName false
        Network.Main.main dataPath busName chainParams "" false "" [] false false
        Wallet.Main.main dataPath busName chain RunActor Wallet.Main.Wipe.NoWipe
        Api.Main.main chain busName apiUri
    ]
    |> List.rev
    |> List.map Disposables.toDisposable
    |> Disposables.fromList

let customChainActor chainParams =
    let chain = Chain.getChain chainParams
    [
        createBroker ()
        Blockchain.Main.main dataPath chainParams busName false
        Network.Main.main dataPath busName chainParams "" false "" [] false false
        Wallet.Main.main dataPath busName chain RunActor Wallet.Main.Wipe.NoWipe
        AddressDB.Main.main dataPath busName chain RunActor AddressDB.Main.NoWipe
        Api.Main.main chain busName apiUri
    ]
    |> List.rev
    |> List.map Disposables.toDisposable
    |> Disposables.fromList

let customChainBasicActor chainParams =
    let chain = Chain.getChain chainParams
    [
        createBroker ()
        Blockchain.Main.main dataPath chainParams busName false
        Network.Main.main dataPath busName chainParams "" false "" [] false false
        Wallet.Main.main dataPath busName chain RunActor Wallet.Main.Wipe.NoWipe
        AddressDB.Main.main dataPath busName chain RunActor AddressDB.Main.NoWipe
        Api.Main.main chain busName apiUri
    ]
    |> List.rev
    |> List.map Disposables.toDisposable
    |> Disposables.fromList

let split value seperators =
    (value:String).Split seperators
    |> Array.choose (fun value ->
        if String.IsNullOrWhiteSpace value then
            None
        else
            Some value
    )

let private uri action = sprintf "http://%s/%s" apiUri action

let private check (response:HttpResponse) =
    response.StatusCode
    |> should equal 200
    response

let private getBody (response:HttpResponse) =
    match response.Body with
    | Text string -> string
    | _ -> String.Empty

let get action =
    action
    |> uri
    |> Http.Request
    |> check
    |> getBody

let post action (jsonValue:JsonValue) =
    action
    |> uri
    |> jsonValue.Request
    |> check
    |> getBody
