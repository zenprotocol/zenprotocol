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
let ``send and recv Transaction``() =
    let msg = Transaction ("Captcha Diem"B)

    use server = Socket.dealer ()
    Socket.bind server "inproc://Transaction.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Transaction.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Transaction size fits stream ``() =
    let transaction:Transaction =
        "Captcha Diem"B

    let messageSize = Transaction.getMessageSize transaction

    let stream =
        Stream.create messageSize
        |> Transaction.write transaction

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv SendAddress``() =
    let msg = SendAddress {
        peerId = Array.create 4 123uy;
        address = "Life is short but Now lasts for ever";
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
        address = "Life is short but Now lasts for ever";
    }

    let messageSize = SendAddress.getMessageSize sendaddress

    let stream =
        Stream.create messageSize
        |> SendAddress.write sendaddress

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Address``() =
    let msg = Address "Life is short but Now lasts for ever"

    use server = Socket.dealer ()
    Socket.bind server "inproc://Address.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Address.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Address size fits stream ``() =
    let address:Address =
        "Life is short but Now lasts for ever"

    let messageSize = Address.getMessageSize address

    let stream =
        Stream.create messageSize
        |> Address.write address

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
    let msg = Addresses ["Name: Brutus";"Age: 43"]

    use server = Socket.dealer ()
    Socket.bind server "inproc://Addresses.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Addresses.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Addresses size fits stream ``() =
    let addresses:Addresses =
        ["Name: Brutus";"Age: 43"]

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
        addresses = ["Name: Brutus";"Age: 43"];
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
        addresses = ["Name: Brutus";"Age: 43"];
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
let ``send and recv GetTransaction``() =
    let msg = GetTransaction {
        peerId = Array.create 4 123uy;
        txHash = Array.create 32 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetTransaction.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetTransaction.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``GetTransaction size fits stream ``() =
    let gettransaction:GetTransaction = {
        peerId = Array.create 4 123uy;
        txHash = Array.create 32 123uy;
    }

    let messageSize = GetTransaction.getMessageSize gettransaction

    let stream =
        Stream.create messageSize
        |> GetTransaction.write gettransaction

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv SendTransaction``() =
    let msg = SendTransaction {
        peerId = Array.create 4 123uy;
        tx = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://SendTransaction.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://SendTransaction.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``SendTransaction size fits stream ``() =
    let sendtransaction:SendTransaction = {
        peerId = Array.create 4 123uy;
        tx = "Captcha Diem"B;
    }

    let messageSize = SendTransaction.getMessageSize sendtransaction

    let stream =
        Stream.create messageSize
        |> SendTransaction.write sendtransaction

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Block``() =
    let msg = Block ("Captcha Diem"B)

    use server = Socket.dealer ()
    Socket.bind server "inproc://Block.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Block.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Block size fits stream ``() =
    let block:Block =
        "Captcha Diem"B

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
let ``send and recv GetNewBlock``() =
    let msg = GetNewBlock {
        peerId = Array.create 4 123uy;
        blockHash = Array.create 32 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetNewBlock.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetNewBlock.test"

    Network.Transport.InProcMessage.send server msg

    let msg' = Network.Transport.InProcMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``GetNewBlock size fits stream ``() =
    let getnewblock:GetNewBlock = {
        peerId = Array.create 4 123uy;
        blockHash = Array.create 32 123uy;
    }

    let messageSize = GetNewBlock.getMessageSize getnewblock

    let stream =
        Stream.create messageSize
        |> GetNewBlock.write getnewblock

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
    let msg = PublishAddressToAll "Life is short but Now lasts for ever"

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
        "Life is short but Now lasts for ever"

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
        blockHash = Array.create 32 123uy;
        numberOfHeaders = 123us;
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
        blockHash = Array.create 32 123uy;
        numberOfHeaders = 123us;
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
        blockHash = Array.create 32 123uy;
        numberOfHeaders = 123us;
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
        blockHash = Array.create 32 123uy;
        numberOfHeaders = 123us;
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
let ``malformed message return None``() =
    use server = Socket.dealer ()
    Socket.bind server "inproc://InProcMessage.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://InProcMessage.test"

    Frame.send server "hello world"B

    let msg = Network.Transport.InProcMessage.recv client
    msg |> should equal None
 