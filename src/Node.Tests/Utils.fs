module Node.Tests.Utils

open Config
open System
open FsNetMQ
open FSharp.Data
open Api.Types
open FsUnit
open Consensus
open Infrastructure

module Actor = FsNetMQ.Actor

let createBroker () =
     Actor.create (fun shim ->
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim

        use sbBroker = ServiceBus.Broker.create poller busName
        use evBroker = EventBus.Broker.create poller busName

        Actor.signal shim
        Poller.run poller
    )

let getActors () =
    [
        createBroker ()
        Blockchain.Main.main dataPath chainParams busName false
        Network.Main.main dataPath busName chainParams "" false "" [] false false
        Wallet.Main.main dataPath busName chain Wallet.Main.Wipe.NoWipe
        AddressDB.Main.main dataPath busName chain AddressDB.Main.NoWipe        
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
