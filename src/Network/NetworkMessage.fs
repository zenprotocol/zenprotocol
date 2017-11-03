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

type Ping = {
        nonce : uint32
    }

type Pong = {
        nonce : uint32
    }

type UnknownPeer = {
        dummy : byte
    }

type UnknownMessage = {
        messageId : byte
    }

type T =
    | Hello of Hello
    | HelloAck of HelloAck
    | Ping of Ping
    | Pong of Pong
    | UnknownPeer of UnknownPeer
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
        0 +
            4 +
            0

    let write (msg:Ping) stream =
        stream
        |> Stream.writeNumber4 msg.nonce

    let read =
        reader {
            let! nonce = Stream.readNumber4

            return ({
                        nonce = nonce;
                    }: Ping)
        }

module Pong =
    let getMessageSize (msg:Pong) =
        0 +
            4 +
            0

    let write (msg:Pong) stream =
        stream
        |> Stream.writeNumber4 msg.nonce

    let read =
        reader {
            let! nonce = Stream.readNumber4

            return ({
                        nonce = nonce;
                    }: Pong)
        }

module UnknownPeer =
    let getMessageSize (msg:UnknownPeer) =
        0 +
            1 +
            0

    let write (msg:UnknownPeer) stream =
        stream
        |> Stream.writeNumber1 msg.dummy

    let read =
        reader {
            let! dummy = Stream.readNumber1

            return ({
                        dummy = dummy;
                    }: UnknownPeer)
        }

module UnknownMessage =
    let getMessageSize (msg:UnknownMessage) =
        0 +
            1 +
            0

    let write (msg:UnknownMessage) stream =
        stream
        |> Stream.writeNumber1 msg.messageId

    let read =
        reader {
            let! messageId = Stream.readNumber1

            return ({
                        messageId = messageId;
                    }: UnknownMessage)
        }


let recv socket =
    let stream, more = Stream.recv socket

    // Drop the rest if any
    if more then Multipart.skip socket

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
        | UnknownPeerMessageId ->
            match UnknownPeer.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (UnknownPeer msg), stream
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

let send socket msg =
    let writeMessage = function
        | Hello msg -> Hello.write msg
        | HelloAck msg -> HelloAck.write msg
        | Ping msg -> Ping.write msg
        | Pong msg -> Pong.write msg
        | UnknownPeer msg -> UnknownPeer.write msg
        | UnknownMessage msg -> UnknownMessage.write msg

    let messageId =
        match msg with
        | Hello _ -> HelloMessageId
        | HelloAck _ -> HelloAckMessageId
        | Ping _ -> PingMessageId
        | Pong _ -> PongMessageId
        | UnknownPeer _ -> UnknownPeerMessageId
        | UnknownMessage _ -> UnknownMessageMessageId

    let messageSize =
        match msg with
        | Hello msg -> Hello.getMessageSize msg
        | HelloAck msg -> HelloAck.getMessageSize msg
        | Ping msg -> Ping.getMessageSize msg
        | Pong msg -> Pong.getMessageSize msg
        | UnknownPeer msg -> UnknownPeer.getMessageSize msg
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

