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
[<LiteralAttribute>]
let SendAddressMessageId = 11uy
[<LiteralAttribute>]
let AddressMessageId = 12uy
[<LiteralAttribute>]
let GetAddressesMessageId = 13uy
[<LiteralAttribute>]
let AddressesMessageId = 14uy
[<LiteralAttribute>]
let SendAddressesMessageId = 15uy
[<LiteralAttribute>]
let GetMemPoolMessageId = 16uy
[<LiteralAttribute>]
let MemPoolMessageId = 17uy
[<LiteralAttribute>]
let GetTransactionMessageId = 18uy
[<LiteralAttribute>]
let SendTransactionMessageId = 19uy
[<LiteralAttribute>]
let BlockMessageId = 20uy
[<LiteralAttribute>]
let BlockHeaderMessageId = 21uy
[<LiteralAttribute>]
let GetBlockMessageId = 22uy
[<LiteralAttribute>]
let BlockRequestMessageId = 23uy
[<LiteralAttribute>]
let SendBlockMessageId = 24uy
[<LiteralAttribute>]
let SendTipMessageId = 25uy
[<LiteralAttribute>]
let GetTipMessageId = 26uy

type Connect =
        string

type Connected = {
        address : string
        peerId : byte[]
    }

type Accepted =
        byte[]

type Disconnected =
        string

type Transaction =
        byte[]

type SendAddress = {
        peerId : byte[]
        address : string
    }

type Address =
        string

type GetAddresses =
        byte[]

type Addresses =
        string list

type SendAddresses = {
        peerId : byte[]
        addresses : string list
    }

type GetMemPool =
        byte[]

type MemPool = {
        peerId : byte[]
        txs : byte[]
    }

type GetTransaction = {
        peerId : byte[]
        txHash : byte[]
    }

type SendTransaction = {
        peerId : byte[]
        tx : byte[]
    }

type Block =
        byte[]

type BlockHeader =
        byte[]

type GetBlock =
        byte[]

type BlockRequest = {
        peerId : byte[]
        blockHash : byte[]
    }

type SendBlock = {
        peerId : byte[]
        block : byte[]
    }

type SendTip = {
        peerId : byte[]
        blockHeader : byte[]
    }

type GetTip =
        byte[]

type T =
    | Connect of Connect
    | Connected of Connected
    | Accepted of Accepted
    | Disconnected of Disconnected
    | Transaction of Transaction
    | SendAddress of SendAddress
    | Address of Address
    | GetAddresses of GetAddresses
    | Addresses of Addresses
    | SendAddresses of SendAddresses
    | GetMemPool of GetMemPool
    | MemPool of MemPool
    | GetTransaction of GetTransaction
    | SendTransaction of SendTransaction
    | Block of Block
    | BlockHeader of BlockHeader
    | GetBlock of GetBlock
    | BlockRequest of BlockRequest
    | SendBlock of SendBlock
    | SendTip of SendTip
    | GetTip of GetTip

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
    let peerIdSize = 4
    let getMessageSize (msg:Connected) =
        0 +
            4 + String.length msg.address +
            4 +
            0

    let write (msg:Connected) stream =
        stream
        |> Stream.writeLongString msg.address
        |> Stream.writeBytes msg.peerId 4

    let read =
        reader {
            let! address = Stream.readLongString
            let! peerId = Stream.readBytes 4

            return ({
                        address = address;
                        peerId = peerId;
                    }: Connected)
        }

module Accepted =
    let peerIdSize = 4
    let getMessageSize (msg:Accepted) =
            4

    let write (msg:Accepted) stream =
        stream
        |> Stream.writeBytes msg 4

    let read =
        reader {
            let! msg = Stream.readBytes 4

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


module SendAddress =
    let peerIdSize = 4
    let getMessageSize (msg:SendAddress) =
        0 +
            4 +
            4 + String.length msg.address +
            0

    let write (msg:SendAddress) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeLongString msg.address

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! address = Stream.readLongString

            return ({
                        peerId = peerId;
                        address = address;
                    }: SendAddress)
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

module GetAddresses =
    let peerIdSize = 4
    let getMessageSize (msg:GetAddresses) =
            4

    let write (msg:GetAddresses) stream =
        stream
        |> Stream.writeBytes msg 4

    let read =
        reader {
            let! msg = Stream.readBytes 4

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


module SendAddresses =
    let peerIdSize = 4
    let getMessageSize (msg:SendAddresses) =
        0 +
            4 +
            List.fold (fun state (value:string) -> state + 4 + Encoding.UTF8.GetByteCount (value)) 4 msg.addresses +
            0

    let write (msg:SendAddresses) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeStrings msg.addresses

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! addresses = Stream.readStrings

            return ({
                        peerId = peerId;
                        addresses = addresses;
                    }: SendAddresses)
        }

module GetMemPool =
    let peerIdSize = 4
    let getMessageSize (msg:GetMemPool) =
            4

    let write (msg:GetMemPool) stream =
        stream
        |> Stream.writeBytes msg 4

    let read =
        reader {
            let! msg = Stream.readBytes 4

            return msg
        }


module MemPool =
    let peerIdSize = 4
    let getMessageSize (msg:MemPool) =
        0 +
            4 +
            4 + Array.length msg.txs +
            0

    let write (msg:MemPool) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeNumber4 (uint32 (Array.length msg.txs))
        |> Stream.writeBytes msg.txs (Array.length msg.txs)

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! txsLength = Stream.readNumber4
            let! txs = Stream.readBytes (int txsLength)

            return ({
                        peerId = peerId;
                        txs = txs;
                    }: MemPool)
        }


module GetTransaction =
    let peerIdSize = 4
    let txHashSize = 32
    let getMessageSize (msg:GetTransaction) =
        0 +
            4 +
            32 +
            0

    let write (msg:GetTransaction) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeBytes msg.txHash 32

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! txHash = Stream.readBytes 32

            return ({
                        peerId = peerId;
                        txHash = txHash;
                    }: GetTransaction)
        }


module SendTransaction =
    let peerIdSize = 4
    let getMessageSize (msg:SendTransaction) =
        0 +
            4 +
            4 + Array.length msg.tx +
            0

    let write (msg:SendTransaction) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeNumber4 (uint32 (Array.length msg.tx))
        |> Stream.writeBytes msg.tx (Array.length msg.tx)

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! txLength = Stream.readNumber4
            let! tx = Stream.readBytes (int txLength)

            return ({
                        peerId = peerId;
                        tx = tx;
                    }: SendTransaction)
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

module BlockHeader =
    let blockHeaderSize = 100
    let getMessageSize (msg:BlockHeader) =
            100

    let write (msg:BlockHeader) stream =
        stream
        |> Stream.writeBytes msg 100

    let read =
        reader {
            let! msg = Stream.readBytes 100

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


module BlockRequest =
    let peerIdSize = 4
    let blockHashSize = 32
    let getMessageSize (msg:BlockRequest) =
        0 +
            4 +
            32 +
            0

    let write (msg:BlockRequest) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeBytes msg.blockHash 32

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! blockHash = Stream.readBytes 32

            return ({
                        peerId = peerId;
                        blockHash = blockHash;
                    }: BlockRequest)
        }


module SendBlock =
    let peerIdSize = 4
    let getMessageSize (msg:SendBlock) =
        0 +
            4 +
            4 + Array.length msg.block +
            0

    let write (msg:SendBlock) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeNumber4 (uint32 (Array.length msg.block))
        |> Stream.writeBytes msg.block (Array.length msg.block)

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! blockLength = Stream.readNumber4
            let! block = Stream.readBytes (int blockLength)

            return ({
                        peerId = peerId;
                        block = block;
                    }: SendBlock)
        }


module SendTip =
    let peerIdSize = 4
    let blockHeaderSize = 100
    let getMessageSize (msg:SendTip) =
        0 +
            4 +
            100 +
            0

    let write (msg:SendTip) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeBytes msg.blockHeader 100

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! blockHeader = Stream.readBytes 100

            return ({
                        peerId = peerId;
                        blockHeader = blockHeader;
                    }: SendTip)
        }

module GetTip =
    let peerIdSize = 4
    let getMessageSize (msg:GetTip) =
            4

    let write (msg:GetTip) stream =
        stream
        |> Stream.writeBytes msg 4

    let read =
        reader {
            let! msg = Stream.readBytes 4

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
            match Accepted.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Accepted msg), stream
        | DisconnectedMessageId ->
            match Disconnected.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Disconnected msg), stream
        | TransactionMessageId ->
            match Transaction.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Transaction msg), stream
        | SendAddressMessageId ->
            match SendAddress.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (SendAddress msg), stream
        | AddressMessageId ->
            match Address.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Address msg), stream
        | GetAddressesMessageId ->
            match GetAddresses.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetAddresses msg), stream
        | AddressesMessageId ->
            match Addresses.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Addresses msg), stream
        | SendAddressesMessageId ->
            match SendAddresses.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (SendAddresses msg), stream
        | GetMemPoolMessageId ->
            match GetMemPool.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetMemPool msg), stream
        | MemPoolMessageId ->
            match MemPool.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (MemPool msg), stream
        | GetTransactionMessageId ->
            match GetTransaction.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetTransaction msg), stream
        | SendTransactionMessageId ->
            match SendTransaction.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (SendTransaction msg), stream
        | BlockMessageId ->
            match Block.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Block msg), stream
        | BlockHeaderMessageId ->
            match BlockHeader.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (BlockHeader msg), stream
        | GetBlockMessageId ->
            match GetBlock.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetBlock msg), stream
        | BlockRequestMessageId ->
            match BlockRequest.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (BlockRequest msg), stream
        | SendBlockMessageId ->
            match SendBlock.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (SendBlock msg), stream
        | SendTipMessageId ->
            match SendTip.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (SendTip msg), stream
        | GetTipMessageId ->
            match GetTip.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetTip msg), stream
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
        | Accepted msg -> Accepted.write msg
        | Disconnected msg -> Disconnected.write msg
        | Transaction msg -> Transaction.write msg
        | SendAddress msg -> SendAddress.write msg
        | Address msg -> Address.write msg
        | GetAddresses msg -> GetAddresses.write msg
        | Addresses msg -> Addresses.write msg
        | SendAddresses msg -> SendAddresses.write msg
        | GetMemPool msg -> GetMemPool.write msg
        | MemPool msg -> MemPool.write msg
        | GetTransaction msg -> GetTransaction.write msg
        | SendTransaction msg -> SendTransaction.write msg
        | Block msg -> Block.write msg
        | BlockHeader msg -> BlockHeader.write msg
        | GetBlock msg -> GetBlock.write msg
        | BlockRequest msg -> BlockRequest.write msg
        | SendBlock msg -> SendBlock.write msg
        | SendTip msg -> SendTip.write msg
        | GetTip msg -> GetTip.write msg

    let messageId =
        match msg with
        | Connect _ -> ConnectMessageId
        | Connected _ -> ConnectedMessageId
        | Accepted _ -> AcceptedMessageId
        | Disconnected _ -> DisconnectedMessageId
        | Transaction _ -> TransactionMessageId
        | SendAddress _ -> SendAddressMessageId
        | Address _ -> AddressMessageId
        | GetAddresses _ -> GetAddressesMessageId
        | Addresses _ -> AddressesMessageId
        | SendAddresses _ -> SendAddressesMessageId
        | GetMemPool _ -> GetMemPoolMessageId
        | MemPool _ -> MemPoolMessageId
        | GetTransaction _ -> GetTransactionMessageId
        | SendTransaction _ -> SendTransactionMessageId
        | Block _ -> BlockMessageId
        | BlockHeader _ -> BlockHeaderMessageId
        | GetBlock _ -> GetBlockMessageId
        | BlockRequest _ -> BlockRequestMessageId
        | SendBlock _ -> SendBlockMessageId
        | SendTip _ -> SendTipMessageId
        | GetTip _ -> GetTipMessageId

    let messageSize =
        match msg with
        | Connect msg -> Connect.getMessageSize msg
        | Connected msg -> Connected.getMessageSize msg
        | Accepted msg -> Accepted.getMessageSize msg
        | Disconnected msg -> Disconnected.getMessageSize msg
        | Transaction msg -> Transaction.getMessageSize msg
        | SendAddress msg -> SendAddress.getMessageSize msg
        | Address msg -> Address.getMessageSize msg
        | GetAddresses msg -> GetAddresses.getMessageSize msg
        | Addresses msg -> Addresses.getMessageSize msg
        | SendAddresses msg -> SendAddresses.getMessageSize msg
        | GetMemPool msg -> GetMemPool.getMessageSize msg
        | MemPool msg -> MemPool.getMessageSize msg
        | GetTransaction msg -> GetTransaction.getMessageSize msg
        | SendTransaction msg -> SendTransaction.getMessageSize msg
        | Block msg -> Block.getMessageSize msg
        | BlockHeader msg -> BlockHeader.getMessageSize msg
        | GetBlock msg -> GetBlock.getMessageSize msg
        | BlockRequest msg -> BlockRequest.getMessageSize msg
        | SendBlock msg -> SendBlock.getMessageSize msg
        | SendTip msg -> SendTip.getMessageSize msg
        | GetTip msg -> GetTip.getMessageSize msg

    //  Signature + message ID + message size
    let frameSize = 2 + 1 + messageSize
    let stream = Stream.create frameSize

    let stream' =
        stream
        |> Stream.writeNumber2 (0xAAA0us ||| 19634us)
        |> Stream.writeNumber1 messageId
        |> writeMessage msg

    Stream.send socket stream'

