module Network.Transport.InProcMessage

open System
open System.Text
open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

[<LiteralAttribute>]
let ConnectMessageId = 1uy
[<LiteralAttribute>]
let ConnectedMessageId = 2uy
[<LiteralAttribute>]
let AcceptedMessageId = 3uy
[<LiteralAttribute>]
let DisconnectedMessageId = 4uy
[<LiteralAttribute>]
let TransactionMessageId = 10uy

type Connect =
        string

type Connected =
        string


type Disconnected =
        string

type Transaction =
        byte[]

type T =
    | Connect of Connect
    | Connected of Connected
    | Accepted
    | Disconnected of Disconnected
    | Transaction of Transaction

module Connect =
    let getMessageSize (msg:Connect) =
            4 + String.length msg

    let write (msg:Connect) stream =
        stream
        |> Stream.writeLongString msg

    let read =
        reader {
            let! msg = Stream.readLongString

            return msg
        }

module Connected =
    let getMessageSize (msg:Connected) =
            4 + String.length msg

    let write (msg:Connected) stream =
        stream
        |> Stream.writeLongString msg

    let read =
        reader {
            let! msg = Stream.readLongString

            return msg
        }

module Disconnected =
    let getMessageSize (msg:Disconnected) =
            4 + String.length msg

    let write (msg:Disconnected) stream =
        stream
        |> Stream.writeLongString msg

    let read =
        reader {
            let! msg = Stream.readLongString

            return msg
        }

module Transaction =
    let getMessageSize (msg:Transaction) =
            4 + Array.length msg

    let write (msg:Transaction) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg))
        |> Stream.writeBytes msg (Array.length msg)

    let read =
        reader {
            let! msgLength = Stream.readNumber4
            let! msg = Stream.readBytes (int msgLength)

            return msg
        }


let private decode stream =
    let readMessage messageId stream =
            match messageId with
        | ConnectMessageId ->
            match Connect.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Connect msg), stream
        | ConnectedMessageId ->
            match Connected.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Connected msg), stream
        | AcceptedMessageId ->
            Some Accepted, stream
        | DisconnectedMessageId ->
            match Disconnected.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Disconnected msg), stream
        | TransactionMessageId ->
            match Transaction.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Transaction msg), stream
        | _ -> None, stream

    let r = reader {
                let! signature = Stream.readNumber2
                do! check (signature = (0xAAA0us ||| 19634us))
                let! messageId = Stream.readNumber1
                let! message = readMessage messageId
                return message
            }

    run r stream

let recv socket =
    let stream, more = Stream.recv socket

    // Drop the rest if any
    if more then Multipart.skip socket

    decode stream

let tryRecv socket timeout =
    match Stream.tryRecv socket timeout with
    | None -> None
    | Some (stream, more) ->
        // Drop the rest if any
        if more then Multipart.skip socket

        decode stream

let send socket msg =
    let writeMessage = function
        | Connect msg -> Connect.write msg
        | Connected msg -> Connected.write msg
        | Accepted -> id
        | Disconnected msg -> Disconnected.write msg
        | Transaction msg -> Transaction.write msg

    let messageId =
        match msg with
        | Connect _ -> ConnectMessageId
        | Connected _ -> ConnectedMessageId
        | Accepted _ -> AcceptedMessageId
        | Disconnected _ -> DisconnectedMessageId
        | Transaction _ -> TransactionMessageId

    let messageSize =
        match msg with
        | Connect msg -> Connect.getMessageSize msg
        | Connected msg -> Connected.getMessageSize msg
        | Accepted -> 0
        | Disconnected msg -> Disconnected.getMessageSize msg
        | Transaction msg -> Transaction.getMessageSize msg

    //  Signature + message ID + message size
    let frameSize = 2 + 1 + messageSize
    let stream = Stream.create frameSize

    let stream' =
        stream
        |> Stream.writeNumber2 (0xAAA0us ||| 19634us)
        |> Stream.writeNumber1 messageId
        |> writeMessage msg

    Stream.send socket stream'

