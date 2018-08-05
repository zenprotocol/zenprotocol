module Node.Tests.Setup

open FsNetMQ
open Infrastructure
open Consensus
open Messaging.Services
open Messaging.Events
open Helpers

module Actor = FsNetMQ.Actor

let busName = "test"
let chain = Chain.Local
let chainParams = Chain.getChainParameters chain
let dataPath = ".data"
let apiUri = "127.0.0.1:29555"

let getUri =
    sprintf "http://%s/"

let clean() =
    Platform.cleanDirectory dataPath

let createBroker() =
     Actor.create (fun shim ->
        use poller = Poller.create ()
        use emObserver = Poller.registerEndMessage poller shim

        use sbBroker = ServiceBus.Broker.create poller busName
        use evBroker = EventBus.Broker.create poller busName

        Actor.signal shim
        Poller.run poller) |> ignore

let initGenesis() =
    printfn "111"
    let block = Block.createGenesis chainParams [Transaction.rootTx] (0UL,0UL)
    let client = ServiceBus.Client.create busName
    Blockchain.validateBlock client block
    
    printfn "1411"
    let subscriber = EventBus.Subscriber.create<Event> busName
    waitForBk subscriber block
    printfn "112"
    
let initBlockchainActor() =
    Blockchain.Main.main dataPath chainParams busName
    |> ignore
    
let initWalletActor() =
    Wallet.Main.main dataPath busName chain
    |> ignore
    
let initApiActor() =
    Api.Main.main chain busName apiUri
    |> ignore
    