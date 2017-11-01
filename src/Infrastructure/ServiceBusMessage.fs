module ServiceBusMessage

open System
open System.Text
open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

[<LiteralAttribute>]
let RegisterMessageId = 1uy
[<LiteralAttribute>]
let CommandMessageId = 2uy
[<LiteralAttribute>]
let RelayCommandMessageId = 3uy
[<LiteralAttribute>]
let RequestMessageId = 4uy
[<LiteralAttribute>]
let RelayRequestMessageId = 5uy
[<LiteralAttribute>]
let ResponseMessageId = 6uy
[<LiteralAttribute>]
let RelayResponseMessageId = 7uy
[<LiteralAttribute>]
let AckMessageId = 8uy

type Register = {
        service : string
    }

type Command = {
        service : string
        payload : byte[]
    }

type RelayCommand = {
        payload : byte[]
    }

type Request = {
        service : string
        payload : byte[]
    }

type RelayRequest = {
        sender : byte[]
        payload : byte[]
    }

type Response = {
        sender : byte[]
        payload : byte[]
    }

type RelayResponse = {
        payload : byte[]
    }

type Ack = {
        dummy : byte
    }

type T =
    | Register of Register
    | Command of Command
    | RelayCommand of RelayCommand
    | Request of Request
    | RelayRequest of RelayRequest
    | Response of Response
    | RelayResponse of RelayResponse
    | Ack of Ack

module Register =
    let getMessageSize (msg:Register) =
        0 +
            1 + String.length msg.service +
            0

    let write (msg:Register) stream =
        stream
        |> Stream.writeString msg.service

    let read =
        reader {
            let! service = Stream.readString

            return ({
                        service = service;
                    }: Register)
        }

module Command =
    let getMessageSize (msg:Command) =
        0 +
            1 + String.length msg.service +
            4 + Array.length msg.payload +
            0

    let write (msg:Command) stream =
        stream
        |> Stream.writeString msg.service
        |> Stream.writeNumber4 (uint32 (Array.length msg.payload))
        |> Stream.writeBytes msg.payload (Array.length msg.payload)

    let read =
        reader {
            let! service = Stream.readString
            let! payloadLength = Stream.readNumber4
            let! payload = Stream.readBytes (int payloadLength)

            return ({
                        service = service;
                        payload = payload;
                    }: Command)
        }

module RelayCommand =
    let getMessageSize (msg:RelayCommand) =
        0 +
            4 + Array.length msg.payload +
            0

    let write (msg:RelayCommand) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg.payload))
        |> Stream.writeBytes msg.payload (Array.length msg.payload)

    let read =
        reader {
            let! payloadLength = Stream.readNumber4
            let! payload = Stream.readBytes (int payloadLength)

            return ({
                        payload = payload;
                    }: RelayCommand)
        }

module Request =
    let getMessageSize (msg:Request) =
        0 +
            1 + String.length msg.service +
            4 + Array.length msg.payload +
            0

    let write (msg:Request) stream =
        stream
        |> Stream.writeString msg.service
        |> Stream.writeNumber4 (uint32 (Array.length msg.payload))
        |> Stream.writeBytes msg.payload (Array.length msg.payload)

    let read =
        reader {
            let! service = Stream.readString
            let! payloadLength = Stream.readNumber4
            let! payload = Stream.readBytes (int payloadLength)

            return ({
                        service = service;
                        payload = payload;
                    }: Request)
        }

module RelayRequest =
    let getMessageSize (msg:RelayRequest) =
        0 +
            4 + Array.length msg.sender +
            4 + Array.length msg.payload +
            0

    let write (msg:RelayRequest) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg.sender))
        |> Stream.writeBytes msg.sender (Array.length msg.sender)
        |> Stream.writeNumber4 (uint32 (Array.length msg.payload))
        |> Stream.writeBytes msg.payload (Array.length msg.payload)

    let read =
        reader {
            let! senderLength = Stream.readNumber4
            let! sender = Stream.readBytes (int senderLength)
            let! payloadLength = Stream.readNumber4
            let! payload = Stream.readBytes (int payloadLength)

            return ({
                        sender = sender;
                        payload = payload;
                    }: RelayRequest)
        }

module Response =
    let getMessageSize (msg:Response) =
        0 +
            4 + Array.length msg.sender +
            4 + Array.length msg.payload +
            0

    let write (msg:Response) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg.sender))
        |> Stream.writeBytes msg.sender (Array.length msg.sender)
        |> Stream.writeNumber4 (uint32 (Array.length msg.payload))
        |> Stream.writeBytes msg.payload (Array.length msg.payload)

    let read =
        reader {
            let! senderLength = Stream.readNumber4
            let! sender = Stream.readBytes (int senderLength)
            let! payloadLength = Stream.readNumber4
            let! payload = Stream.readBytes (int payloadLength)

            return ({
                        sender = sender;
                        payload = payload;
                    }: Response)
        }

module RelayResponse =
    let getMessageSize (msg:RelayResponse) =
        0 +
            4 + Array.length msg.payload +
            0

    let write (msg:RelayResponse) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg.payload))
        |> Stream.writeBytes msg.payload (Array.length msg.payload)

    let read =
        reader {
            let! payloadLength = Stream.readNumber4
            let! payload = Stream.readBytes (int payloadLength)

            return ({
                        payload = payload;
                    }: RelayResponse)
        }

module Ack =
    let getMessageSize (msg:Ack) =
        0 +
            1 +
            0

    let write (msg:Ack) stream =
        stream
        |> Stream.writeNumber1 msg.dummy

    let read =
        reader {
            let! dummy = Stream.readNumber1

            return ({
                        dummy = dummy;
                    }: Ack)
        }


let recv socket =
    let stream, more = Stream.recv socket

    // Drop the rest if any
    if more then Multipart.skip socket

    let readMessage messageId stream =
        match messageId with
        | RegisterMessageId ->
            match Register.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Register msg), stream
        | CommandMessageId ->
            match Command.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Command msg), stream
        | RelayCommandMessageId ->
            match RelayCommand.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (RelayCommand msg), stream
        | RequestMessageId ->
            match Request.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Request msg), stream
        | RelayRequestMessageId ->
            match RelayRequest.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (RelayRequest msg), stream
        | ResponseMessageId ->
            match Response.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Response msg), stream
        | RelayResponseMessageId ->
            match RelayResponse.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (RelayResponse msg), stream
        | AckMessageId ->
            match Ack.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Ack msg), stream
        | _ -> None, stream

    let r = reader {
                let! signature = Stream.readNumber2
                do! check (signature = (0xAAA0us ||| 0us))
                let! messageId = Stream.readNumber1
                let! message = readMessage messageId
                return message
            }

    run r stream

let send msg socket =
    let writeMessage = function
        | Register msg -> Register.write msg
        | Command msg -> Command.write msg
        | RelayCommand msg -> RelayCommand.write msg
        | Request msg -> Request.write msg
        | RelayRequest msg -> RelayRequest.write msg
        | Response msg -> Response.write msg
        | RelayResponse msg -> RelayResponse.write msg
        | Ack msg -> Ack.write msg

    let messageId =
        match msg with
        | Register _ -> RegisterMessageId
        | Command _ -> CommandMessageId
        | RelayCommand _ -> RelayCommandMessageId
        | Request _ -> RequestMessageId
        | RelayRequest _ -> RelayRequestMessageId
        | Response _ -> ResponseMessageId
        | RelayResponse _ -> RelayResponseMessageId
        | Ack _ -> AckMessageId

    let messageSize =
        match msg with
        | Register msg -> Register.getMessageSize msg
        | Command msg -> Command.getMessageSize msg
        | RelayCommand msg -> RelayCommand.getMessageSize msg
        | Request msg -> Request.getMessageSize msg
        | RelayRequest msg -> RelayRequest.getMessageSize msg
        | Response msg -> Response.getMessageSize msg
        | RelayResponse msg -> RelayResponse.getMessageSize msg
        | Ack msg -> Ack.getMessageSize msg

    //  Signature + message ID + message size
    let frameSize = 2 + 1 + messageSize
    let stream = Stream.create frameSize

    let stream' =
        stream
        |> Stream.writeNumber2 (0xAAA0us ||| 0us)
        |> Stream.writeNumber1 messageId
        |> writeMessage msg

    Stream.send stream' socket

