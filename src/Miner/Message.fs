module Miner.Message

open System
open System.Text
open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

[<LiteralAttribute>]
let NewTemplateMessageId = 1uy
[<LiteralAttribute>]
let StopMessageId = 2uy
[<LiteralAttribute>]
let ExitMessageId = 3uy

type NewTemplate =
        byte[]



type T =
    | NewTemplate of NewTemplate
    | Stop
    | Exit

module NewTemplate =
    let getMessageSize (msg:NewTemplate) =
            4 + Array.length msg

    let write (msg:NewTemplate) stream =
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
            | NewTemplateMessageId ->
                match NewTemplate.read stream with
                | None,stream -> None,stream
                | Some msg, stream -> Some (NewTemplate msg), stream
            | StopMessageId ->
                Some Stop, stream
            | ExitMessageId ->
                Some Exit, stream
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
        | NewTemplate msg -> NewTemplate.write msg
        | Stop -> id
        | Exit -> id

    let messageId =
        match msg with
        | NewTemplate _ -> NewTemplateMessageId
        | Stop _ -> StopMessageId
        | Exit _ -> ExitMessageId

    let messageSize =
        match msg with
        | NewTemplate msg -> NewTemplate.getMessageSize msg
        | Stop -> 0
        | Exit -> 0

    //  Signature + message ID + message size
    let frameSize = 2 + 1 + messageSize
    let stream = Stream.create frameSize

    let stream' =
        stream
        |> Stream.writeNumber2 (0xAAA0us ||| 19317us)
        |> Stream.writeNumber1 messageId
        |> writeMessage msg

    Stream.send socket stream'

