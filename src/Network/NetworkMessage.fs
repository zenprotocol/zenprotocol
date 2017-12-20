module Network.Message

open System
open System.Text
open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

[<LiteralAttribute>]
let HelloMessageId = 1uy
[<LiteralAttribute>]
let HelloAckMessageId = 2uy
[<LiteralAttribute>]
let PingMessageId = 3uy
[<LiteralAttribute>]
let PongMessageId = 4uy
[<LiteralAttribute>]
let TransactionMessageId = 5uy
[<LiteralAttribute>]
let AddressMessageId = 6uy
[<LiteralAttribute>]
let UnknownPeerMessageId = 100uy
[<LiteralAttribute>]
let UnknownMessageMessageId = 101uy

type Hello = {
        network : uint32
        version : uint32
    }

type HelloAck = {
        network : uint32
        version : uint32
    }

type Ping =
        uint32

type Pong =
        uint32

type Transaction =
        byte[]

type Address =
        string


type UnknownMessage =
        byte

type T =
    | Hello of Hello
    | HelloAck of HelloAck
    | Ping of Ping
    | Pong of Pong
    | Transaction of Transaction
    | Address of Address
    | UnknownPeer
    | UnknownMessage of UnknownMessage


module Hello =
    let getMessageSize (msg:Hello) =
        0 +
            4 +
            4 +
            0

    let write (msg:Hello) stream =
        stream
        |> Stream.writeNumber4 msg.network
        |> Stream.writeNumber4 msg.version

    let read =
        reader {
            let! network = Stream.readNumber4
            let! version = Stream.readNumber4

            return ({
                        network = network;
                        version = version;
                    }: Hello)
        }


module HelloAck =
    let getMessageSize (msg:HelloAck) =
        0 +
            4 +
            4 +
            0

    let write (msg:HelloAck) stream =
        stream
        |> Stream.writeNumber4 msg.network
        |> Stream.writeNumber4 msg.version

    let read =
        reader {
            let! network = Stream.readNumber4
            let! version = Stream.readNumber4

            return ({
                        network = network;
                        version = version;
                    }: HelloAck)
        }

module Ping =
    let getMessageSize (msg:Ping) =
            4

    let write (msg:Ping) stream =
        stream
        |> Stream.writeNumber4 msg

    let read =
        reader {
            let! msg = Stream.readNumber4

            return msg
        }

module Pong =
    let getMessageSize (msg:Pong) =
            4

    let write (msg:Pong) stream =
        stream
        |> Stream.writeNumber4 msg

    let read =
        reader {
            let! msg = Stream.readNumber4

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

module Address =
    let getMessageSize (msg:Address) =
            4 + String.length msg

    let write (msg:Address) stream =
        stream
        |> Stream.writeLongString msg

    let read =
        reader {
            let! msg = Stream.readLongString

            return msg
        }

module UnknownMessage =
    let getMessageSize (msg:UnknownMessage) =
            1

    let write (msg:UnknownMessage) stream =
        stream
        |> Stream.writeNumber1 msg

    let read =
        reader {
            let! msg = Stream.readNumber1

            return msg
        }


let private decode stream =
    let readMessage messageId stream =
            match messageId with
        | HelloMessageId ->
            match Hello.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Hello msg), stream
        | HelloAckMessageId ->
            match HelloAck.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (HelloAck msg), stream
        | PingMessageId ->
            match Ping.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Ping msg), stream
        | PongMessageId ->
            match Pong.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Pong msg), stream
        | TransactionMessageId ->
            match Transaction.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Transaction msg), stream
        | AddressMessageId ->
            match Address.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Address msg), stream
        | UnknownPeerMessageId ->
            Some UnknownPeer, stream
        | UnknownMessageMessageId ->
            match UnknownMessage.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (UnknownMessage msg), stream
        | _ -> None, stream

    let r = reader {
                let! signature = Stream.readNumber2
                do! check (signature = (0xAAA0us ||| 19317us))
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
        | Hello msg -> Hello.write msg
        | HelloAck msg -> HelloAck.write msg
        | Ping msg -> Ping.write msg
        | Pong msg -> Pong.write msg
        | Transaction msg -> Transaction.write msg
        | Address msg -> Address.write msg
        | UnknownPeer -> id
        | UnknownMessage msg -> UnknownMessage.write msg

    let messageId =
        match msg with
        | Hello _ -> HelloMessageId
        | HelloAck _ -> HelloAckMessageId
        | Ping _ -> PingMessageId
        | Pong _ -> PongMessageId
        | Transaction _ -> TransactionMessageId
        | Address _ -> AddressMessageId
        | UnknownPeer _ -> UnknownPeerMessageId
        | UnknownMessage _ -> UnknownMessageMessageId

    let messageSize =
        match msg with
        | Hello msg -> Hello.getMessageSize msg
        | HelloAck msg -> HelloAck.getMessageSize msg
        | Ping msg -> Ping.getMessageSize msg
        | Pong msg -> Pong.getMessageSize msg
        | Transaction msg -> Transaction.getMessageSize msg
        | Address msg -> Address.getMessageSize msg
        | UnknownPeer -> 0
        | UnknownMessage msg -> UnknownMessage.getMessageSize msg

    //  Signature + message ID + message size
    let frameSize = 2 + 1 + messageSize
    let stream = Stream.create frameSize

    let stream' =
        stream
        |> Stream.writeNumber2 (0xAAA0us ||| 19317us)
        |> Stream.writeNumber1 messageId
        |> writeMessage msg

    Stream.send socket stream'

