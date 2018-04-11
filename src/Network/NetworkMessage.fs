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
let GetAddressesMessageId = 7uy
[<LiteralAttribute>]
let AddressesMessageId = 8uy
[<LiteralAttribute>]
let GetMemPoolMessageId = 9uy
[<LiteralAttribute>]
let MemPoolMessageId = 10uy
[<LiteralAttribute>]
let GetTransactionMessageId = 11uy
[<LiteralAttribute>]
let GetBlockMessageId = 12uy
[<LiteralAttribute>]
let BlockMessageId = 13uy
[<LiteralAttribute>]
let GetTipMessageId = 14uy
[<LiteralAttribute>]
let TipMessageId = 15uy
[<LiteralAttribute>]
let NewBlockMessageId = 16uy
[<LiteralAttribute>]
let GetHeadersMessageId = 17uy
[<LiteralAttribute>]
let HeadersMessageId = 18uy
[<LiteralAttribute>]
let UnknownPeerMessageId = 100uy
[<LiteralAttribute>]
let UnknownMessageMessageId = 101uy
[<LiteralAttribute>]
let IncorrectNetworkMessageId = 102uy

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


type Addresses =
        string list


type MemPool =
        byte[]

type GetTransaction =
        byte[]

type GetBlock =
        byte[]

type Block =
        byte[]


type Tip =
        byte[]

type NewBlock =
        byte[]

type GetHeaders = {
        from : byte[]
        endHash : byte[]
    }

type Headers =
        byte[]


type UnknownMessage =
        byte


type T =
    | Hello of Hello
    | HelloAck of HelloAck
    | Ping of Ping
    | Pong of Pong
    | Transaction of Transaction
    | Address of Address
    | GetAddresses
    | Addresses of Addresses
    | GetMemPool
    | MemPool of MemPool
    | GetTransaction of GetTransaction
    | GetBlock of GetBlock
    | Block of Block
    | GetTip
    | Tip of Tip
    | NewBlock of NewBlock
    | GetHeaders of GetHeaders
    | Headers of Headers
    | UnknownPeer
    | UnknownMessage of UnknownMessage
    | IncorrectNetwork


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

module Addresses =
    let getMessageSize (msg:Addresses) =
            List.fold (fun state (value:string) -> state + 4 + Encoding.UTF8.GetByteCount (value)) 4 msg

    let write (msg:Addresses) stream =
        stream
        |> Stream.writeStrings msg

    let read =
        reader {
            let! msg = Stream.readStrings

            return msg
        }

module MemPool =
    let getMessageSize (msg:MemPool) =
            4 + Array.length msg

    let write (msg:MemPool) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg))
        |> Stream.writeBytes msg (Array.length msg)

    let read =
        reader {
            let! msgLength = Stream.readNumber4
            let! msg = Stream.readBytes (int msgLength)

            return msg
        }

module GetTransaction =
    let txHashSize = 32
    let getMessageSize (msg:GetTransaction) =
            32

    let write (msg:GetTransaction) stream =
        stream
        |> Stream.writeBytes msg 32

    let read =
        reader {
            let! msg = Stream.readBytes 32

            return msg
        }

module GetBlock =
    let blockHashSize = 32
    let getMessageSize (msg:GetBlock) =
            32

    let write (msg:GetBlock) stream =
        stream
        |> Stream.writeBytes msg 32

    let read =
        reader {
            let! msg = Stream.readBytes 32

            return msg
        }

module Block =
    let getMessageSize (msg:Block) =
            4 + Array.length msg

    let write (msg:Block) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg))
        |> Stream.writeBytes msg (Array.length msg)

    let read =
        reader {
            let! msgLength = Stream.readNumber4
            let! msg = Stream.readBytes (int msgLength)

            return msg
        }

module Tip =
    let blockHeaderSize = 100
    let getMessageSize (msg:Tip) =
            100

    let write (msg:Tip) stream =
        stream
        |> Stream.writeBytes msg 100

    let read =
        reader {
            let! msg = Stream.readBytes 100

            return msg
        }

module NewBlock =
    let blockHeaderSize = 100
    let getMessageSize (msg:NewBlock) =
            100

    let write (msg:NewBlock) stream =
        stream
        |> Stream.writeBytes msg 100

    let read =
        reader {
            let! msg = Stream.readBytes 100

            return msg
        }


module GetHeaders =
    let endHashSize = 32
    let getMessageSize (msg:GetHeaders) =
        0 +
            4 + Array.length msg.from +
            32 +
            0

    let write (msg:GetHeaders) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg.from))
        |> Stream.writeBytes msg.from (Array.length msg.from)
        |> Stream.writeBytes msg.endHash 32

    let read =
        reader {
            let! fromLength = Stream.readNumber4
            let! from = Stream.readBytes (int fromLength)
            let! endHash = Stream.readBytes 32

            return ({
                        from = from;
                        endHash = endHash;
                    }: GetHeaders)
        }

module Headers =
    let getMessageSize (msg:Headers) =
            4 + Array.length msg

    let write (msg:Headers) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg))
        |> Stream.writeBytes msg (Array.length msg)

    let read =
        reader {
            let! msgLength = Stream.readNumber4
            let! msg = Stream.readBytes (int msgLength)

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
        | GetAddressesMessageId ->
            Some GetAddresses, stream
        | AddressesMessageId ->
            match Addresses.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Addresses msg), stream
        | GetMemPoolMessageId ->
            Some GetMemPool, stream
        | MemPoolMessageId ->
            match MemPool.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (MemPool msg), stream
        | GetTransactionMessageId ->
            match GetTransaction.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetTransaction msg), stream
        | GetBlockMessageId ->
            match GetBlock.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetBlock msg), stream
        | BlockMessageId ->
            match Block.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Block msg), stream
        | GetTipMessageId ->
            Some GetTip, stream
        | TipMessageId ->
            match Tip.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Tip msg), stream
        | NewBlockMessageId ->
            match NewBlock.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (NewBlock msg), stream
        | GetHeadersMessageId ->
            match GetHeaders.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetHeaders msg), stream
        | HeadersMessageId ->
            match Headers.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Headers msg), stream
        | UnknownPeerMessageId ->
            Some UnknownPeer, stream
        | UnknownMessageMessageId ->
            match UnknownMessage.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (UnknownMessage msg), stream
        | IncorrectNetworkMessageId ->
            Some IncorrectNetwork, stream
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
        | GetAddresses -> id
        | Addresses msg -> Addresses.write msg
        | GetMemPool -> id
        | MemPool msg -> MemPool.write msg
        | GetTransaction msg -> GetTransaction.write msg
        | GetBlock msg -> GetBlock.write msg
        | Block msg -> Block.write msg
        | GetTip -> id
        | Tip msg -> Tip.write msg
        | NewBlock msg -> NewBlock.write msg
        | GetHeaders msg -> GetHeaders.write msg
        | Headers msg -> Headers.write msg
        | UnknownPeer -> id
        | UnknownMessage msg -> UnknownMessage.write msg
        | IncorrectNetwork -> id

    let messageId =
        match msg with
        | Hello _ -> HelloMessageId
        | HelloAck _ -> HelloAckMessageId
        | Ping _ -> PingMessageId
        | Pong _ -> PongMessageId
        | Transaction _ -> TransactionMessageId
        | Address _ -> AddressMessageId
        | GetAddresses _ -> GetAddressesMessageId
        | Addresses _ -> AddressesMessageId
        | GetMemPool _ -> GetMemPoolMessageId
        | MemPool _ -> MemPoolMessageId
        | GetTransaction _ -> GetTransactionMessageId
        | GetBlock _ -> GetBlockMessageId
        | Block _ -> BlockMessageId
        | GetTip _ -> GetTipMessageId
        | Tip _ -> TipMessageId
        | NewBlock _ -> NewBlockMessageId
        | GetHeaders _ -> GetHeadersMessageId
        | Headers _ -> HeadersMessageId
        | UnknownPeer _ -> UnknownPeerMessageId
        | UnknownMessage _ -> UnknownMessageMessageId
        | IncorrectNetwork _ -> IncorrectNetworkMessageId

    let messageSize =
        match msg with
        | Hello msg -> Hello.getMessageSize msg
        | HelloAck msg -> HelloAck.getMessageSize msg
        | Ping msg -> Ping.getMessageSize msg
        | Pong msg -> Pong.getMessageSize msg
        | Transaction msg -> Transaction.getMessageSize msg
        | Address msg -> Address.getMessageSize msg
        | GetAddresses -> 0
        | Addresses msg -> Addresses.getMessageSize msg
        | GetMemPool -> 0
        | MemPool msg -> MemPool.getMessageSize msg
        | GetTransaction msg -> GetTransaction.getMessageSize msg
        | GetBlock msg -> GetBlock.getMessageSize msg
        | Block msg -> Block.getMessageSize msg
        | GetTip -> 0
        | Tip msg -> Tip.getMessageSize msg
        | NewBlock msg -> NewBlock.getMessageSize msg
        | GetHeaders msg -> GetHeaders.getMessageSize msg
        | Headers msg -> Headers.getMessageSize msg
        | UnknownPeer -> 0
        | UnknownMessage msg -> UnknownMessage.getMessageSize msg
        | IncorrectNetwork -> 0

    //  Signature + message ID + message size
    let frameSize = 2 + 1 + messageSize
    let stream = Stream.create frameSize

    let stream' =
        stream
        |> Stream.writeNumber2 (0xAAA0us ||| 19317us)
        |> Stream.writeNumber1 messageId
        |> writeMessage msg

    Stream.send socket stream'

