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
let TransactionsMessageId = 10uy
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
let GetTransactionsMessageId = 18uy
[<LiteralAttribute>]
let SendTransactionsMessageId = 19uy
[<LiteralAttribute>]
let BlockMessageId = 20uy
[<LiteralAttribute>]
let TipMessageId = 21uy
[<LiteralAttribute>]
let PublishBlockMessageId = 22uy
[<LiteralAttribute>]
let NewBlockMessageId = 23uy
[<LiteralAttribute>]
let GetBlockMessageId = 24uy
[<LiteralAttribute>]
let GetBlockFromMessageId = 25uy
[<LiteralAttribute>]
let BlockRequestMessageId = 26uy
[<LiteralAttribute>]
let SendBlockMessageId = 27uy
[<LiteralAttribute>]
let SendTipMessageId = 28uy
[<LiteralAttribute>]
let GetTipMessageId = 29uy
[<LiteralAttribute>]
let PublishAddressToAllMessageId = 30uy
[<LiteralAttribute>]
let GetHeadersMessageId = 31uy
[<LiteralAttribute>]
let HeadersRequestMessageId = 32uy
[<LiteralAttribute>]
let SendHeadersMessageId = 33uy
[<LiteralAttribute>]
let HeadersMessageId = 34uy
[<LiteralAttribute>]
let DisconnectPeerMessageId = 35uy
[<LiteralAttribute>]
let PublishTransactionsMessageId = 36uy
[<LiteralAttribute>]
let NewTransactionsMessageId = 37uy
[<LiteralAttribute>]
let GetTipFromAllPeersMessageId = 38uy

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

type Transactions = {
        count : uint32
        txs : byte[]
    }

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

type GetTransactions = {
        peerId : byte[]
        txHashes : byte[]
    }

type SendTransactions = {
        peerId : byte[]
        count : uint32
        txs : byte[]
    }

type Block = {
        peerId : byte[]
        block : byte[]
    }

type Tip = {
        peerId : byte[]
        blockHeader : byte[]
    }

type PublishBlock =
        byte[]

type NewBlock = {
        blockHeader : byte[]
        peerId : byte[]
    }

type GetBlock =
        byte[]

type GetBlockFrom = {
        peerId : byte[]
        blockHash : byte[]
    }

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

type PublishAddressToAll =
        string

type GetHeaders = {
        peerId : byte[]
        from : byte[]
        endHash : byte[]
    }

type HeadersRequest = {
        peerId : byte[]
        from : byte[]
        endHash : byte[]
    }

type SendHeaders = {
        peerId : byte[]
        headers : byte[]
    }

type Headers = {
        peerId : byte[]
        headers : byte[]
    }

type DisconnectPeer =
        byte[]

type PublishTransactions =
        byte[]

type NewTransactions = {
        peerId : byte[]
        txHashes : byte[]
    }


type T =
    | Connect of Connect
    | Connected of Connected
    | Accepted of Accepted
    | Disconnected of Disconnected
    | Transactions of Transactions
    | SendAddress of SendAddress
    | Address of Address
    | GetAddresses of GetAddresses
    | Addresses of Addresses
    | SendAddresses of SendAddresses
    | GetMemPool of GetMemPool
    | MemPool of MemPool
    | GetTransactions of GetTransactions
    | SendTransactions of SendTransactions
    | Block of Block
    | Tip of Tip
    | PublishBlock of PublishBlock
    | NewBlock of NewBlock
    | GetBlock of GetBlock
    | GetBlockFrom of GetBlockFrom
    | BlockRequest of BlockRequest
    | SendBlock of SendBlock
    | SendTip of SendTip
    | GetTip of GetTip
    | PublishAddressToAll of PublishAddressToAll
    | GetHeaders of GetHeaders
    | HeadersRequest of HeadersRequest
    | SendHeaders of SendHeaders
    | Headers of Headers
    | DisconnectPeer of DisconnectPeer
    | PublishTransactions of PublishTransactions
    | NewTransactions of NewTransactions
    | GetTipFromAllPeers

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


module Transactions =
    let getMessageSize (msg:Transactions) =
        0 +
            4 +
            4 + Array.length msg.txs +
            0

    let write (msg:Transactions) stream =
        stream
        |> Stream.writeNumber4 msg.count
        |> Stream.writeNumber4 (uint32 (Array.length msg.txs))
        |> Stream.writeBytes msg.txs (Array.length msg.txs)

    let read =
        reader {
            let! count = Stream.readNumber4
            let! txsLength = Stream.readNumber4
            let! txs = Stream.readBytes (int txsLength)

            return ({
                        count = count;
                        txs = txs;
                    }: Transactions)
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


module GetTransactions =
    let peerIdSize = 4
    let getMessageSize (msg:GetTransactions) =
        0 +
            4 +
            4 + Array.length msg.txHashes +
            0

    let write (msg:GetTransactions) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeNumber4 (uint32 (Array.length msg.txHashes))
        |> Stream.writeBytes msg.txHashes (Array.length msg.txHashes)

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! txHashesLength = Stream.readNumber4
            let! txHashes = Stream.readBytes (int txHashesLength)

            return ({
                        peerId = peerId;
                        txHashes = txHashes;
                    }: GetTransactions)
        }


module SendTransactions =
    let peerIdSize = 4
    let getMessageSize (msg:SendTransactions) =
        0 +
            4 +
            4 +
            4 + Array.length msg.txs +
            0

    let write (msg:SendTransactions) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeNumber4 msg.count
        |> Stream.writeNumber4 (uint32 (Array.length msg.txs))
        |> Stream.writeBytes msg.txs (Array.length msg.txs)

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! count = Stream.readNumber4
            let! txsLength = Stream.readNumber4
            let! txs = Stream.readBytes (int txsLength)

            return ({
                        peerId = peerId;
                        count = count;
                        txs = txs;
                    }: SendTransactions)
        }


module Block =
    let peerIdSize = 4
    let getMessageSize (msg:Block) =
        0 +
            4 +
            4 + Array.length msg.block +
            0

    let write (msg:Block) stream =
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
                    }: Block)
        }


module Tip =
    let peerIdSize = 4
    let blockHeaderSize = 100
    let getMessageSize (msg:Tip) =
        0 +
            4 +
            100 +
            0

    let write (msg:Tip) stream =
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
                    }: Tip)
        }

module PublishBlock =
    let blockHeaderSize = 100
    let getMessageSize (msg:PublishBlock) =
            100

    let write (msg:PublishBlock) stream =
        stream
        |> Stream.writeBytes msg 100

    let read =
        reader {
            let! msg = Stream.readBytes 100

            return msg
        }


module NewBlock =
    let blockHeaderSize = 100
    let peerIdSize = 4
    let getMessageSize (msg:NewBlock) =
        0 +
            100 +
            4 +
            0

    let write (msg:NewBlock) stream =
        stream
        |> Stream.writeBytes msg.blockHeader 100
        |> Stream.writeBytes msg.peerId 4

    let read =
        reader {
            let! blockHeader = Stream.readBytes 100
            let! peerId = Stream.readBytes 4

            return ({
                        blockHeader = blockHeader;
                        peerId = peerId;
                    }: NewBlock)
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


module GetBlockFrom =
    let peerIdSize = 4
    let blockHashSize = 32
    let getMessageSize (msg:GetBlockFrom) =
        0 +
            4 +
            32 +
            0

    let write (msg:GetBlockFrom) stream =
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
                    }: GetBlockFrom)
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

module PublishAddressToAll =
    let getMessageSize (msg:PublishAddressToAll) =
            4 + String.length msg

    let write (msg:PublishAddressToAll) stream =
        stream
        |> Stream.writeLongString msg

    let read =
        reader {
            let! msg = Stream.readLongString

            return msg
        }


module GetHeaders =
    let peerIdSize = 4
    let endHashSize = 32
    let getMessageSize (msg:GetHeaders) =
        0 +
            4 +
            4 + Array.length msg.from +
            32 +
            0

    let write (msg:GetHeaders) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeNumber4 (uint32 (Array.length msg.from))
        |> Stream.writeBytes msg.from (Array.length msg.from)
        |> Stream.writeBytes msg.endHash 32

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! fromLength = Stream.readNumber4
            let! from = Stream.readBytes (int fromLength)
            let! endHash = Stream.readBytes 32

            return ({
                        peerId = peerId;
                        from = from;
                        endHash = endHash;
                    }: GetHeaders)
        }


module HeadersRequest =
    let peerIdSize = 4
    let endHashSize = 32
    let getMessageSize (msg:HeadersRequest) =
        0 +
            4 +
            4 + Array.length msg.from +
            32 +
            0

    let write (msg:HeadersRequest) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeNumber4 (uint32 (Array.length msg.from))
        |> Stream.writeBytes msg.from (Array.length msg.from)
        |> Stream.writeBytes msg.endHash 32

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! fromLength = Stream.readNumber4
            let! from = Stream.readBytes (int fromLength)
            let! endHash = Stream.readBytes 32

            return ({
                        peerId = peerId;
                        from = from;
                        endHash = endHash;
                    }: HeadersRequest)
        }


module SendHeaders =
    let peerIdSize = 4
    let getMessageSize (msg:SendHeaders) =
        0 +
            4 +
            4 + Array.length msg.headers +
            0

    let write (msg:SendHeaders) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeNumber4 (uint32 (Array.length msg.headers))
        |> Stream.writeBytes msg.headers (Array.length msg.headers)

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! headersLength = Stream.readNumber4
            let! headers = Stream.readBytes (int headersLength)

            return ({
                        peerId = peerId;
                        headers = headers;
                    }: SendHeaders)
        }


module Headers =
    let peerIdSize = 4
    let getMessageSize (msg:Headers) =
        0 +
            4 +
            4 + Array.length msg.headers +
            0

    let write (msg:Headers) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeNumber4 (uint32 (Array.length msg.headers))
        |> Stream.writeBytes msg.headers (Array.length msg.headers)

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! headersLength = Stream.readNumber4
            let! headers = Stream.readBytes (int headersLength)

            return ({
                        peerId = peerId;
                        headers = headers;
                    }: Headers)
        }

module DisconnectPeer =
    let peerIdSize = 4
    let getMessageSize (msg:DisconnectPeer) =
            4

    let write (msg:DisconnectPeer) stream =
        stream
        |> Stream.writeBytes msg 4

    let read =
        reader {
            let! msg = Stream.readBytes 4

            return msg
        }

module PublishTransactions =
    let getMessageSize (msg:PublishTransactions) =
            4 + Array.length msg

    let write (msg:PublishTransactions) stream =
        stream
        |> Stream.writeNumber4 (uint32 (Array.length msg))
        |> Stream.writeBytes msg (Array.length msg)

    let read =
        reader {
            let! msgLength = Stream.readNumber4
            let! msg = Stream.readBytes (int msgLength)

            return msg
        }


module NewTransactions =
    let peerIdSize = 4
    let getMessageSize (msg:NewTransactions) =
        0 +
            4 +
            4 + Array.length msg.txHashes +
            0

    let write (msg:NewTransactions) stream =
        stream
        |> Stream.writeBytes msg.peerId 4
        |> Stream.writeNumber4 (uint32 (Array.length msg.txHashes))
        |> Stream.writeBytes msg.txHashes (Array.length msg.txHashes)

    let read =
        reader {
            let! peerId = Stream.readBytes 4
            let! txHashesLength = Stream.readNumber4
            let! txHashes = Stream.readBytes (int txHashesLength)

            return ({
                        peerId = peerId;
                        txHashes = txHashes;
                    }: NewTransactions)
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
        | TransactionsMessageId ->
            match Transactions.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Transactions msg), stream
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
        | GetTransactionsMessageId ->
            match GetTransactions.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetTransactions msg), stream
        | SendTransactionsMessageId ->
            match SendTransactions.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (SendTransactions msg), stream
        | BlockMessageId ->
            match Block.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Block msg), stream
        | TipMessageId ->
            match Tip.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Tip msg), stream
        | PublishBlockMessageId ->
            match PublishBlock.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (PublishBlock msg), stream
        | NewBlockMessageId ->
            match NewBlock.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (NewBlock msg), stream
        | GetBlockMessageId ->
            match GetBlock.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetBlock msg), stream
        | GetBlockFromMessageId ->
            match GetBlockFrom.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetBlockFrom msg), stream
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
        | PublishAddressToAllMessageId ->
            match PublishAddressToAll.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (PublishAddressToAll msg), stream
        | GetHeadersMessageId ->
            match GetHeaders.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (GetHeaders msg), stream
        | HeadersRequestMessageId ->
            match HeadersRequest.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (HeadersRequest msg), stream
        | SendHeadersMessageId ->
            match SendHeaders.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (SendHeaders msg), stream
        | HeadersMessageId ->
            match Headers.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (Headers msg), stream
        | DisconnectPeerMessageId ->
            match DisconnectPeer.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (DisconnectPeer msg), stream
        | PublishTransactionsMessageId ->
            match PublishTransactions.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (PublishTransactions msg), stream
        | NewTransactionsMessageId ->
            match NewTransactions.read stream with
            | None,stream -> None,stream
            | Some msg, stream -> Some (NewTransactions msg), stream
        | GetTipFromAllPeersMessageId ->
            Some GetTipFromAllPeers, stream
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
        | Transactions msg -> Transactions.write msg
        | SendAddress msg -> SendAddress.write msg
        | Address msg -> Address.write msg
        | GetAddresses msg -> GetAddresses.write msg
        | Addresses msg -> Addresses.write msg
        | SendAddresses msg -> SendAddresses.write msg
        | GetMemPool msg -> GetMemPool.write msg
        | MemPool msg -> MemPool.write msg
        | GetTransactions msg -> GetTransactions.write msg
        | SendTransactions msg -> SendTransactions.write msg
        | Block msg -> Block.write msg
        | Tip msg -> Tip.write msg
        | PublishBlock msg -> PublishBlock.write msg
        | NewBlock msg -> NewBlock.write msg
        | GetBlock msg -> GetBlock.write msg
        | GetBlockFrom msg -> GetBlockFrom.write msg
        | BlockRequest msg -> BlockRequest.write msg
        | SendBlock msg -> SendBlock.write msg
        | SendTip msg -> SendTip.write msg
        | GetTip msg -> GetTip.write msg
        | PublishAddressToAll msg -> PublishAddressToAll.write msg
        | GetHeaders msg -> GetHeaders.write msg
        | HeadersRequest msg -> HeadersRequest.write msg
        | SendHeaders msg -> SendHeaders.write msg
        | Headers msg -> Headers.write msg
        | DisconnectPeer msg -> DisconnectPeer.write msg
        | PublishTransactions msg -> PublishTransactions.write msg
        | NewTransactions msg -> NewTransactions.write msg
        | GetTipFromAllPeers -> id

    let messageId =
        match msg with
        | Connect _ -> ConnectMessageId
        | Connected _ -> ConnectedMessageId
        | Accepted _ -> AcceptedMessageId
        | Disconnected _ -> DisconnectedMessageId
        | Transactions _ -> TransactionsMessageId
        | SendAddress _ -> SendAddressMessageId
        | Address _ -> AddressMessageId
        | GetAddresses _ -> GetAddressesMessageId
        | Addresses _ -> AddressesMessageId
        | SendAddresses _ -> SendAddressesMessageId
        | GetMemPool _ -> GetMemPoolMessageId
        | MemPool _ -> MemPoolMessageId
        | GetTransactions _ -> GetTransactionsMessageId
        | SendTransactions _ -> SendTransactionsMessageId
        | Block _ -> BlockMessageId
        | Tip _ -> TipMessageId
        | PublishBlock _ -> PublishBlockMessageId
        | NewBlock _ -> NewBlockMessageId
        | GetBlock _ -> GetBlockMessageId
        | GetBlockFrom _ -> GetBlockFromMessageId
        | BlockRequest _ -> BlockRequestMessageId
        | SendBlock _ -> SendBlockMessageId
        | SendTip _ -> SendTipMessageId
        | GetTip _ -> GetTipMessageId
        | PublishAddressToAll _ -> PublishAddressToAllMessageId
        | GetHeaders _ -> GetHeadersMessageId
        | HeadersRequest _ -> HeadersRequestMessageId
        | SendHeaders _ -> SendHeadersMessageId
        | Headers _ -> HeadersMessageId
        | DisconnectPeer _ -> DisconnectPeerMessageId
        | PublishTransactions _ -> PublishTransactionsMessageId
        | NewTransactions _ -> NewTransactionsMessageId
        | GetTipFromAllPeers _ -> GetTipFromAllPeersMessageId

    let messageSize =
        match msg with
        | Connect msg -> Connect.getMessageSize msg
        | Connected msg -> Connected.getMessageSize msg
        | Accepted msg -> Accepted.getMessageSize msg
        | Disconnected msg -> Disconnected.getMessageSize msg
        | Transactions msg -> Transactions.getMessageSize msg
        | SendAddress msg -> SendAddress.getMessageSize msg
        | Address msg -> Address.getMessageSize msg
        | GetAddresses msg -> GetAddresses.getMessageSize msg
        | Addresses msg -> Addresses.getMessageSize msg
        | SendAddresses msg -> SendAddresses.getMessageSize msg
        | GetMemPool msg -> GetMemPool.getMessageSize msg
        | MemPool msg -> MemPool.getMessageSize msg
        | GetTransactions msg -> GetTransactions.getMessageSize msg
        | SendTransactions msg -> SendTransactions.getMessageSize msg
        | Block msg -> Block.getMessageSize msg
        | Tip msg -> Tip.getMessageSize msg
        | PublishBlock msg -> PublishBlock.getMessageSize msg
        | NewBlock msg -> NewBlock.getMessageSize msg
        | GetBlock msg -> GetBlock.getMessageSize msg
        | GetBlockFrom msg -> GetBlockFrom.getMessageSize msg
        | BlockRequest msg -> BlockRequest.getMessageSize msg
        | SendBlock msg -> SendBlock.getMessageSize msg
        | SendTip msg -> SendTip.getMessageSize msg
        | GetTip msg -> GetTip.getMessageSize msg
        | PublishAddressToAll msg -> PublishAddressToAll.getMessageSize msg
        | GetHeaders msg -> GetHeaders.getMessageSize msg
        | HeadersRequest msg -> HeadersRequest.getMessageSize msg
        | SendHeaders msg -> SendHeaders.getMessageSize msg
        | Headers msg -> Headers.getMessageSize msg
        | DisconnectPeer msg -> DisconnectPeer.getMessageSize msg
        | PublishTransactions msg -> PublishTransactions.getMessageSize msg
        | NewTransactions msg -> NewTransactions.getMessageSize msg
        | GetTipFromAllPeers -> 0

    //  Signature + message ID + message size
    let frameSize = 2 + 1 + messageSize
    let stream = Stream.create frameSize

    let stream' =
        stream
        |> Stream.writeNumber2 (0xAAA0us ||| 19634us)
        |> Stream.writeNumber1 messageId
        |> writeMessage msg

    Stream.send socket stream'

