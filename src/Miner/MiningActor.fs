module Miner.MiningActor

open Consensus
open Consensus.Chain
open Infrastructure
open FsNetMQ
open Miner
open Consensus.Serialization
open Consensus.Types
open System
open Logary.Message

type private State =
    | Exit
    | WaitForTemplate
    | Continue of Block * (uint64 * uint64)

let random = new System.Random()

let getRandomNonce () =
    let array = Array.zeroCreate 8

    random.NextBytes (array)

    System.BitConverter.ToUInt64 (array,0)

let create (chain:ChainParameters) busName address =
    let findNonce client (template:Block) nonce =
        let mutable found = false
        let mutable attemptsLeft = 200_000 // number of attempts before trying to fetch a message

        let mutable n1 = fst nonce
        let mutable n2 = snd nonce

        let difficulty = Difficulty.uncompress template.header.difficulty

        let headerBytes = Serialization.Header.serialize template.header
        BigEndianBitConverter.putUInt64 n1 headerBytes 84

        while attemptsLeft > 0 && not found do

            if n1 = UInt64.MaxValue then
                n1 <- getRandomNonce()
                n2 <- 0UL

                BigEndianBitConverter.putUInt64 n1 headerBytes 84
            else
                n2 <- n2 + 1UL

            BigEndianBitConverter.putUInt64 n2 headerBytes 92

            let blockHash = Hash.compute headerBytes

            if blockHash <= difficulty then
                eventX "New block mined"
                |> Log.info

                found <- true

                let header = {template.header with nonce=n1,n2 }
                let block = {template with header=header}

                // We found a block
                Messaging.Services.Blockchain.validateMinedBlock client block
                ()
            else
                attemptsLeft <- attemptsLeft - 1

        if found then
            WaitForTemplate
        else
            Continue (template, (n1,n2))

    let handler shim =
        use subscriber = Socket.sub ()
        Socket.subscribe subscriber ""
        Socket.connect subscriber address

        use client = ServiceBus.Client.create busName

        let mutable state = WaitForTemplate

        Actor.signal shim

        while state <> Exit do
            let message =
                match state with
                | WaitForTemplate ->
                    Message.recv subscriber |> Option.get |> Some // This internal procotol, we can assume the message is well formed, we un-option it and option again to get an exception if invalid
                | _ ->
                    Message.tryRecv subscriber 0<milliseconds>

            match message,state with
            | Some Message.Exit,_ ->
                state <- Exit
            | Some Message.Stop, _ ->
                state <- WaitForTemplate
            | Some (Message.NewTemplate template),_ ->
                let template = Block.deserialize template |> Option.get // This internal procotol, we can assume the block is well formed
                state <- findNonce client template (getRandomNonce(),0UL)
            | _, Continue (template,nonce) ->
                state <- findNonce client template nonce
            | _, _ -> ()

    Actor.create handler