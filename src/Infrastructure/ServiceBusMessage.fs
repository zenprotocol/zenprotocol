module Infrastructure.ServiceBusMessage

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

type Register =
        string

type Command = {
        service : string
        payload : byte[]
    }

type RelayCommand =
        byte[]

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

type RelayResponse =
        byte[]


type T =
    | Register of Register
    | Command of Command
    | RelayCommand of RelayCommand
    | Request of Request
    | RelayRequest of RelayRequest
    | Response of Response
    | RelayResponse of RelayResponse
    | Ack

module Register =
    let getMessageSize (msg:Register) =
            1 + String.length msg

    let write (msg:Register) stream =
        stream
        |> Stream.writeString msg

    let read =
        reader {
            let! msg = Stream.readString

            return msg
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
            4 + Array.length msg

    let write (msg:RelayCommand) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg))
        |> Stream.writeBytes msg (Array.length msg)

    let read =
        reader {
            let! msgLength = Stream.readNumber4
            let! msg = Stream.readBytes (int msgLength)

            return msg
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
            4 + Array.length msg

    let write (msg:RelayResponse) stream =
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
            Some Ack, stream
        | _ -> None, stream

    let r = reader {
                let! signature = Stream.readNumber2
                do! check (signature = (0xAAA0us ||| 0us))
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
        | Register msg -> Register.write msg
        | Command msg -> Command.write msg
        | RelayCommand msg -> RelayCommand.write msg
        | Request msg -> Request.write msg
        | RelayRequest msg -> RelayRequest.write msg
        | Response msg -> Response.write msg
        | RelayResponse msg -> RelayResponse.write msg
        | Ack -> id

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
        | Ack -> 0

    //  Signature + message ID + message size
    let frameSize = 2 + 1 + messageSize
    let stream = Stream.create frameSize

    let stream' =
        stream
        |> Stream.writeNumber2 (0xAAA0us ||| 0us)
        |> Stream.writeNumber1 messageId
        |> writeMessage msg

    Stream.send socket stream'

