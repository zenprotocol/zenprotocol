module Network.Tests.InProcMessageTests

open NUnit.Framework
open FsUnit
open FsNetMQ
open Network.Transport.InProcMessage


[<Test>]
let ``send and recv Connect``() =
    let msg = Connect "Life is short but Now lasts for ever"

    use server = Socket.dealer ()
    Socket.bind server "inproc://Connect.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Connect.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Connect size fits stream ``() =
    let connect:Connect =
        "Life is short but Now lasts for ever"

    let messageSize = Connect.getMessageSize connect

    let stream =
        Stream.create messageSize
        |> Connect.write connect

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Connected``() =
    let msg = Connected {
        address = "Life is short but Now lasts for ever";
        peerId = Array.create 4 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Connected.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Connected.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Connected size fits stream ``() =
    let connected:Connected = {
        address = "Life is short but Now lasts for ever";
        peerId = Array.create 4 123uy;
    }

    let messageSize = Connected.getMessageSize connected

    let stream =
        Stream.create messageSize
        |> Connected.write connected

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Accepted``() =
    let msg = Accepted (Array.create 4 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://Accepted.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Accepted.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Accepted size fits stream ``() =
    let accepted:Accepted =
        Array.create 4 123uy

    let messageSize = Accepted.getMessageSize accepted

    let stream =
        Stream.create messageSize
        |> Accepted.write accepted

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Disconnected``() =
    let msg = Disconnected "Life is short but Now lasts for ever"

    use server = Socket.dealer ()
    Socket.bind server "inproc://Disconnected.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Disconnected.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Disconnected size fits stream ``() =
    let disconnected:Disconnected =
        "Life is short but Now lasts for ever"

    let messageSize = Disconnected.getMessageSize disconnected

    let stream =
        Stream.create messageSize
        |> Disconnected.write disconnected

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Transactions``() =
    let msg = Transactions {
        count = 123ul;
        txs = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Transactions.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Transactions.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Transactions size fits stream ``() =
    let transactions:Transactions = {
        count = 123ul;
        txs = "Captcha Diem"B;
    }

    let messageSize = Transactions.getMessageSize transactions

    let stream =
        Stream.create messageSize
        |> Transactions.write transactions

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv SendAddress``() =
    let msg = SendAddress {
        peerId = Array.create 4 123uy;
        addressTimestamp = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://SendAddress.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://SendAddress.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``SendAddress size fits stream ``() =
    let sendaddress:SendAddress = {
        peerId = Array.create 4 123uy;
        addressTimestamp = "Captcha Diem"B;
    }

    let messageSize = SendAddress.getMessageSize sendaddress

    let stream =
        Stream.create messageSize
        |> SendAddress.write sendaddress

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv PublishAddresses``() =
    let msg = PublishAddresses {
        count = 123ul;
        addresses = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://PublishAddresses.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://PublishAddresses.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``PublishAddresses size fits stream ``() =
    let publishaddresses:PublishAddresses = {
        count = 123ul;
        addresses = "Captcha Diem"B;
    }

    let messageSize = PublishAddresses.getMessageSize publishaddresses

    let stream =
        Stream.create messageSize
        |> PublishAddresses.write publishaddresses

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv GetAddresses``() =
    let msg = GetAddresses (Array.create 4 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetAddresses.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetAddresses.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``GetAddresses size fits stream ``() =
    let getaddresses:GetAddresses =
        Array.create 4 123uy

    let messageSize = GetAddresses.getMessageSize getaddresses

    let stream =
        Stream.create messageSize
        |> GetAddresses.write getaddresses

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Addresses``() =
    let msg = Addresses {
        count = 123ul;
        addresses = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Addresses.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Addresses.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Addresses size fits stream ``() =
    let addresses:Addresses = {
        count = 123ul;
        addresses = "Captcha Diem"B;
    }

    let messageSize = Addresses.getMessageSize addresses

    let stream =
        Stream.create messageSize
        |> Addresses.write addresses

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv SendAddresses``() =
    let msg = SendAddresses {
        peerId = Array.create 4 123uy;
        count = 123ul;
        addresses = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://SendAddresses.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://SendAddresses.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``SendAddresses size fits stream ``() =
    let sendaddresses:SendAddresses = {
        peerId = Array.create 4 123uy;
        count = 123ul;
        addresses = "Captcha Diem"B;
    }

    let messageSize = SendAddresses.getMessageSize sendaddresses

    let stream =
        Stream.create messageSize
        |> SendAddresses.write sendaddresses

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv GetMemPool``() =
    let msg = GetMemPool (Array.create 4 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetMemPool.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetMemPool.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``GetMemPool size fits stream ``() =
    let getmempool:GetMemPool =
        Array.create 4 123uy

    let messageSize = GetMemPool.getMessageSize getmempool

    let stream =
        Stream.create messageSize
        |> GetMemPool.write getmempool

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv MemPool``() =
    let msg = MemPool {
        peerId = Array.create 4 123uy;
        txs = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://MemPool.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://MemPool.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``MemPool size fits stream ``() =
    let mempool:MemPool = {
        peerId = Array.create 4 123uy;
        txs = "Captcha Diem"B;
    }

    let messageSize = MemPool.getMessageSize mempool

    let stream =
        Stream.create messageSize
        |> MemPool.write mempool

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv GetTransactions``() =
    let msg = GetTransactions {
        peerId = Array.create 4 123uy;
        txHashes = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetTransactions.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetTransactions.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``GetTransactions size fits stream ``() =
    let gettransactions:GetTransactions = {
        peerId = Array.create 4 123uy;
        txHashes = "Captcha Diem"B;
    }

    let messageSize = GetTransactions.getMessageSize gettransactions

    let stream =
        Stream.create messageSize
        |> GetTransactions.write gettransactions

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv SendTransactions``() =
    let msg = SendTransactions {
        peerId = Array.create 4 123uy;
        count = 123ul;
        txs = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://SendTransactions.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://SendTransactions.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``SendTransactions size fits stream ``() =
    let sendtransactions:SendTransactions = {
        peerId = Array.create 4 123uy;
        count = 123ul;
        txs = "Captcha Diem"B;
    }

    let messageSize = SendTransactions.getMessageSize sendtransactions

    let stream =
        Stream.create messageSize
        |> SendTransactions.write sendtransactions

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Block``() =
    let msg = Block {
        peerId = Array.create 4 123uy;
        block = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Block.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Block.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Block size fits stream ``() =
    let block:Block = {
        peerId = Array.create 4 123uy;
        block = "Captcha Diem"B;
    }

    let messageSize = Block.getMessageSize block

    let stream =
        Stream.create messageSize
        |> Block.write block

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Tip``() =
    let msg = Tip {
        peerId = Array.create 4 123uy;
        blockHeader = Array.create 100 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Tip.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Tip.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Tip size fits stream ``() =
    let tip:Tip = {
        peerId = Array.create 4 123uy;
        blockHeader = Array.create 100 123uy;
    }

    let messageSize = Tip.getMessageSize tip

    let stream =
        Stream.create messageSize
        |> Tip.write tip

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv PublishBlock``() =
    let msg = PublishBlock (Array.create 100 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://PublishBlock.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://PublishBlock.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``PublishBlock size fits stream ``() =
    let publishblock:PublishBlock =
        Array.create 100 123uy

    let messageSize = PublishBlock.getMessageSize publishblock

    let stream =
        Stream.create messageSize
        |> PublishBlock.write publishblock

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv NewBlock``() =
    let msg = NewBlock {
        blockHeader = Array.create 100 123uy;
        peerId = Array.create 4 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://NewBlock.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://NewBlock.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``NewBlock size fits stream ``() =
    let newblock:NewBlock = {
        blockHeader = Array.create 100 123uy;
        peerId = Array.create 4 123uy;
    }

    let messageSize = NewBlock.getMessageSize newblock

    let stream =
        Stream.create messageSize
        |> NewBlock.write newblock

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv GetBlock``() =
    let msg = GetBlock (Array.create 32 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetBlock.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetBlock.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``GetBlock size fits stream ``() =
    let getblock:GetBlock =
        Array.create 32 123uy

    let messageSize = GetBlock.getMessageSize getblock

    let stream =
        Stream.create messageSize
        |> GetBlock.write getblock

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv GetBlockFrom``() =
    let msg = GetBlockFrom {
        peerId = Array.create 4 123uy;
        blockHash = Array.create 32 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetBlockFrom.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetBlockFrom.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``GetBlockFrom size fits stream ``() =
    let getblockfrom:GetBlockFrom = {
        peerId = Array.create 4 123uy;
        blockHash = Array.create 32 123uy;
    }

    let messageSize = GetBlockFrom.getMessageSize getblockfrom

    let stream =
        Stream.create messageSize
        |> GetBlockFrom.write getblockfrom

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv BlockRequest``() =
    let msg = BlockRequest {
        peerId = Array.create 4 123uy;
        blockHash = Array.create 32 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://BlockRequest.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://BlockRequest.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``BlockRequest size fits stream ``() =
    let blockrequest:BlockRequest = {
        peerId = Array.create 4 123uy;
        blockHash = Array.create 32 123uy;
    }

    let messageSize = BlockRequest.getMessageSize blockrequest

    let stream =
        Stream.create messageSize
        |> BlockRequest.write blockrequest

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv SendBlock``() =
    let msg = SendBlock {
        peerId = Array.create 4 123uy;
        block = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://SendBlock.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://SendBlock.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``SendBlock size fits stream ``() =
    let sendblock:SendBlock = {
        peerId = Array.create 4 123uy;
        block = "Captcha Diem"B;
    }

    let messageSize = SendBlock.getMessageSize sendblock

    let stream =
        Stream.create messageSize
        |> SendBlock.write sendblock

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv SendTip``() =
    let msg = SendTip {
        peerId = Array.create 4 123uy;
        blockHeader = Array.create 100 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://SendTip.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://SendTip.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``SendTip size fits stream ``() =
    let sendtip:SendTip = {
        peerId = Array.create 4 123uy;
        blockHeader = Array.create 100 123uy;
    }

    let messageSize = SendTip.getMessageSize sendtip

    let stream =
        Stream.create messageSize
        |> SendTip.write sendtip

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv GetTip``() =
    let msg = GetTip (Array.create 4 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetTip.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetTip.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``GetTip size fits stream ``() =
    let gettip:GetTip =
        Array.create 4 123uy

    let messageSize = GetTip.getMessageSize gettip

    let stream =
        Stream.create messageSize
        |> GetTip.write gettip

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv PublishAddressToAll``() =
    let msg = PublishAddressToAll ("Captcha Diem"B)

    use server = Socket.dealer ()
    Socket.bind server "inproc://PublishAddressToAll.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://PublishAddressToAll.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``PublishAddressToAll size fits stream ``() =
    let publishaddresstoall:PublishAddressToAll =
        "Captcha Diem"B

    let messageSize = PublishAddressToAll.getMessageSize publishaddresstoall

    let stream =
        Stream.create messageSize
        |> PublishAddressToAll.write publishaddresstoall

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv GetHeaders``() =
    let msg = GetHeaders {
        peerId = Array.create 4 123uy;
        from = "Captcha Diem"B;
        endHash = Array.create 32 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetHeaders.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetHeaders.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``GetHeaders size fits stream ``() =
    let getheaders:GetHeaders = {
        peerId = Array.create 4 123uy;
        from = "Captcha Diem"B;
        endHash = Array.create 32 123uy;
    }

    let messageSize = GetHeaders.getMessageSize getheaders

    let stream =
        Stream.create messageSize
        |> GetHeaders.write getheaders

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv HeadersRequest``() =
    let msg = HeadersRequest {
        peerId = Array.create 4 123uy;
        from = "Captcha Diem"B;
        endHash = Array.create 32 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://HeadersRequest.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://HeadersRequest.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``HeadersRequest size fits stream ``() =
    let headersrequest:HeadersRequest = {
        peerId = Array.create 4 123uy;
        from = "Captcha Diem"B;
        endHash = Array.create 32 123uy;
    }

    let messageSize = HeadersRequest.getMessageSize headersrequest

    let stream =
        Stream.create messageSize
        |> HeadersRequest.write headersrequest

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv SendHeaders``() =
    let msg = SendHeaders {
        peerId = Array.create 4 123uy;
        headers = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://SendHeaders.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://SendHeaders.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``SendHeaders size fits stream ``() =
    let sendheaders:SendHeaders = {
        peerId = Array.create 4 123uy;
        headers = "Captcha Diem"B;
    }

    let messageSize = SendHeaders.getMessageSize sendheaders

    let stream =
        Stream.create messageSize
        |> SendHeaders.write sendheaders

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Headers``() =
    let msg = Headers {
        peerId = Array.create 4 123uy;
        headers = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Headers.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Headers.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Headers size fits stream ``() =
    let headers:Headers = {
        peerId = Array.create 4 123uy;
        headers = "Captcha Diem"B;
    }

    let messageSize = Headers.getMessageSize headers

    let stream =
        Stream.create messageSize
        |> Headers.write headers

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv DisconnectPeer``() =
    let msg = DisconnectPeer (Array.create 4 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://DisconnectPeer.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://DisconnectPeer.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``DisconnectPeer size fits stream ``() =
    let disconnectpeer:DisconnectPeer =
        Array.create 4 123uy

    let messageSize = DisconnectPeer.getMessageSize disconnectpeer

    let stream =
        Stream.create messageSize
        |> DisconnectPeer.write disconnectpeer

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv PublishTransactions``() =
    let msg = PublishTransactions ("Captcha Diem"B)

    use server = Socket.dealer ()
    Socket.bind server "inproc://PublishTransactions.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://PublishTransactions.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``PublishTransactions size fits stream ``() =
    let publishtransactions:PublishTransactions =
        "Captcha Diem"B

    let messageSize = PublishTransactions.getMessageSize publishtransactions

    let stream =
        Stream.create messageSize
        |> PublishTransactions.write publishtransactions

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv NewTransactions``() =
    let msg = NewTransactions {
        peerId = Array.create 4 123uy;
        txHashes = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://NewTransactions.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://NewTransactions.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``NewTransactions size fits stream ``() =
    let newtransactions:NewTransactions = {
        peerId = Array.create 4 123uy;
        txHashes = "Captcha Diem"B;
    }

    let messageSize = NewTransactions.getMessageSize newtransactions

    let stream =
        Stream.create messageSize
        |> NewTransactions.write newtransactions

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv GetTipFromAllPeers``() =
    let msg = GetTipFromAllPeers

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetTipFromAllPeers.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetTipFromAllPeers.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)


[<Test>]
let ``send and recv UpdateAddressTimestamp``() =
    let msg = UpdateAddressTimestamp "Life is short but Now lasts for ever"

    use server = Socket.dealer ()
    Socket.bind server "inproc://UpdateAddressTimestamp.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://UpdateAddressTimestamp.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``UpdateAddressTimestamp size fits stream ``() =
    let updateaddresstimestamp:UpdateAddressTimestamp =
        "Life is short but Now lasts for ever"

    let messageSize = UpdateAddressTimestamp.getMessageSize updateaddresstimestamp

    let stream =
        Stream.create messageSize
        |> UpdateAddressTimestamp.write updateaddresstimestamp

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``malformed message return None``() =
    use server = Socket.dealer ()
    Socket.bind server "inproc://InProcMessage.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://InProcMessage.test"

    Frame.send server "hello world"B

    let msg = Network.Transport.InProcMessage.recv client
    msg |> should equal None

