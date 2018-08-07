module Node.Tests.BusTests

open NUnit.Framework
open FsUnit
open Infrastructure
open Consensus
open Consensus.Chain
open Tests.SampleContract
open Messaging.Events
open Api.Types
open Setup
open Messaging.Services
open Helpers

[<OneTimeSetUp>]
let setUp = fun () ->
    printfn "111dddd"
    
    clean()
    createBroker()
    initBlockchainActor()
    initWalletActor()
    initGenesis()

[<TearDown>]
clean()

[<Test>]
let ``Contract should activate and execute``() =
    let client = ServiceBus.Client.create busName
    let subscriber = EventBus.Subscriber.create<Event> busName

    match Wallet.activateContract client sampleContractCode 10ul with
    | Ok (contractActivationTx, cHash) ->
        Blockchain.validateTransaction client contractActivationTx
        waitForTx subscriber contractActivationTx
        match Wallet.executeContract client cHash "" Contract.EmptyData Map.empty with
        | Ok _ -> ()
        | Error error ->
            failwith error
    | Error error ->
        failwith error