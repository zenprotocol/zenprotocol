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
let ``malformed message return None``() =
    use server = Socket.dealer ()
    Socket.bind server "inproc://InProcMessage.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://InProcMessage.test"

    Frame.send server "hello world"B

    let msg = Network.Transport.InProcMessage.recv client
    msg |> should equal None
 