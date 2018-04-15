module Node.Tests.ContractTests

open System
open NUnit.Framework
open FsUnit
open FsNetMQ
open Consensus
open Consensus.Types
open Infrastructure
open Consensus.Chain
open Consensus
open Consensus.Tests.SampleContract
open Messaging.Services
open Messaging.Events
open Api.Types
open Prims

module Actor = FsNetMQ.Actor

let busName = "test"
let chain = Chain.Local
let chainParams = Chain.getChainParameters chain
let dataPath = ".data"
let apiUri = "127.0.0.1:29555"

let createBroker () =
     Actor.create (fun shim ->
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim

        use sbBroker = ServiceBus.Broker.create poller busName
        use evBroker = EventBus.Broker.create poller busName

        Actor.signal shim
        Poller.run poller
    )

let clean() =
    Platform.cleanDirectory dataPath

[<OneTimeSetUp>]
let setUp = fun () ->
    clean ()

    createBroker () |> ignore
    Blockchain.Main.main dataPath chainParams busName |> ignore
    Wallet.Main.main dataPath busName chain |> ignore
    Api.Main.main chain busName apiUri |> ignore

    // initialize genesis block
    let block = Block.createGenesis chainParams [Consensus.Tests.Helper.rootTx] (0UL,0UL)
    let client = ServiceBus.Client.create busName
    Blockchain.validateBlock client block

[<TearDown>]
let tearDown = fun () ->
    clean ()

let rec waitForTx subscriber tx =
    match EventBus.Subscriber.recv subscriber with
    | TransactionAddedToMemPool (_, tx) when tx = tx -> ()
    | _ -> waitForTx subscriber tx

(*
[<Test>]
let ``Contract should activate and execute - Bus``() =
    let client = ServiceBus.Client.create busName
    let subscriber = EventBus.Subscriber.create<Event> busName

    match Wallet.activateContract client sampleContractCode 10ul with
    | Ok (contractActivationTx, cHash) ->
        Blockchain.validateTransaction client contractActivationTx
        waitForTx subscriber contractActivationTx
        match Wallet.executeContract client cHash "" None true None Map.empty with
        | Ok _ -> ()
        | Error error ->
            failwith error
    | Error error ->
        failwith error
*)
//
//[<Test>]
//let ``Contract should activate and execute - API``() =
//    let activate = new ContractActivateRequestJson.Root(sampleContractCode, 10)
//    let response = activate.JsonValue.Request ("http://" + apiUri + "/wallet/contract/activate")
//    response.StatusCode |> should equal 200
//    let responseBody =
//        match response.Body with
//        | FSharp.Data.Text string -> string
//        | _ -> failwith "unexpected response type"
//    let response' = ContractActivateResponseJson.Parse responseBody
//    let execute = new ContractExecuteRequestJson.Root(response'.Address,"", "",new ContractExecuteRequestJson.Options(true, ""), [| new ContractExecuteRequestJson.Spend("", "", int64 0) |])
//    let response = execute.JsonValue.Request ("http://" + apiUri + "/wallet/contract/execute")
//    response.StatusCode |> should equal 200