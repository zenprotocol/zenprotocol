module Miner.Main

open Consensus
open Messaging.Events
open Messaging.Services
open Infrastructure
open Consensus.Types
open Consensus.Difficulty
open Consensus.Serialization
open FSharp
open System
open Logary.Message

let handleEvent client publisher event =
    Wallet.getAddressPKHash client
    |> Result.map (fun pkHash ->
        match event with
        | TransactionAddedToMemPool _
        | TipChanged _ ->
            Blockchain.getBlockTemplate client pkHash
            |> Block.serialize
            |> Message.NewTemplate
            |> Message.send publisher
        | _ -> ())
    |> Result.mapError (fun error ->
        eventX "Miner could not get address: {error}"
        >> setField "error" error
        |> Log.info)
    |> ignore

let main busName chain numberOfThreads =
    Actor.create<unit,unit,Event,unit> busName "Miner" (fun poller sbObservable ebObservable  ->
        let client = ServiceBus.Client.create busName
        let publisher = FsNetMQ.Socket.pub ()

        let address = sprintf "inproc://%s-miner-inproc" busName
        FsNetMQ.Socket.bind publisher address

        let actors =
            [1..numberOfThreads]
            |> List.map (fun _ -> MininingActor.create chain busName address)

        eventX "Miner running"
        |> Log.info

        Wallet.getAddressPKHash client
        |> Result.map (fun pkHash ->
            let template = Blockchain.getBlockTemplate client pkHash |> Block.serialize

            Message.send publisher <| Message.NewTemplate template

            )
        |> Result.mapError (fun error ->
            eventX "Miner could not get address: {error}"
            >> setField "error" error
            |> Log.info)
        |> ignore

        let observable =
            ebObservable
            |> Observable.map (handleEvent client publisher)

        Disposables.fromFunction (fun () ->
            Message.send publisher Message.Exit

            // Dispose all actors
            Disposables.disposeList actors

            Disposables.dispose client
            Disposables.dispose publisher
        )
        , observable
    )