module Network.Tests.MessageTests

open NUnit.Framework
open FsUnit
open FsNetMQ
open Network.Message


[<Test>]
let ``send and recv Hello``() =
    let msg = Hello {
        network = 123ul;
        version = 123ul;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Hello.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Hello.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Hello size fits stream ``() =
    let hello:Hello = {
        network = 123ul;
        version = 123ul;
    }

    let messageSize = Hello.getMessageSize hello

    let stream =
        Stream.create messageSize
        |> Hello.write hello

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv HelloAck``() =
    let msg = HelloAck {
        network = 123ul;
        version = 123ul;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://HelloAck.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://HelloAck.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``HelloAck size fits stream ``() =
    let helloack:HelloAck = {
        network = 123ul;
        version = 123ul;
    }

    let messageSize = HelloAck.getMessageSize helloack

    let stream =
        Stream.create messageSize
        |> HelloAck.write helloack

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Ping``() =
    let msg = Ping 123ul

    use server = Socket.dealer ()
    Socket.bind server "inproc://Ping.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Ping.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Ping size fits stream ``() =
    let ping:Ping =
        123ul

    let messageSize = Ping.getMessageSize ping

    let stream =
        Stream.create messageSize
        |> Ping.write ping

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Pong``() =
    let msg = Pong 123ul

    use server = Socket.dealer ()
    Socket.bind server "inproc://Pong.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Pong.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Pong size fits stream ``() =
    let pong:Pong =
        123ul

    let messageSize = Pong.getMessageSize pong

    let stream =
        Stream.create messageSize
        |> Pong.write pong

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Transaction``() =
    let msg = Transaction ("Captcha Diem"B)

    use server = Socket.dealer ()
    Socket.bind server "inproc://Transaction.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Transaction.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

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
let ``send and recv Address``() =
    let msg = Address "Life is short but Now lasts for ever"

    use server = Socket.dealer ()
    Socket.bind server "inproc://Address.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Address.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

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
    let msg = GetAddresses

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetAddresses.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetAddresses.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)


[<Test>]
let ``send and recv Addresses``() =
    let msg = Addresses ["Name: Brutus";"Age: 43"]

    use server = Socket.dealer ()
    Socket.bind server "inproc://Addresses.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Addresses.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

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
let ``send and recv GetMemPool``() =
    let msg = GetMemPool

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetMemPool.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetMemPool.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)


[<Test>]
let ``send and recv MemPool``() =
    let msg = MemPool ("Captcha Diem"B)

    use server = Socket.dealer ()
    Socket.bind server "inproc://MemPool.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://MemPool.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``MemPool size fits stream ``() =
    let mempool:MemPool =
        "Captcha Diem"B

    let messageSize = MemPool.getMessageSize mempool

    let stream =
        Stream.create messageSize
        |> MemPool.write mempool

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv GetTransaction``() =
    let msg = GetTransaction (Array.create 32 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetTransaction.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetTransaction.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``GetTransaction size fits stream ``() =
    let gettransaction:GetTransaction =
        Array.create 32 123uy

    let messageSize = GetTransaction.getMessageSize gettransaction

    let stream =
        Stream.create messageSize
        |> GetTransaction.write gettransaction

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv GetBlock``() =
    let msg = GetBlock (Array.create 32 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetBlock.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetBlock.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

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
let ``send and recv Block``() =
    let msg = Block ("Captcha Diem"B)

    use server = Socket.dealer ()
    Socket.bind server "inproc://Block.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Block.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

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
let ``send and recv GetTip``() =
    let msg = GetTip

    use server = Socket.dealer ()
    Socket.bind server "inproc://GetTip.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://GetTip.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)


[<Test>]
let ``send and recv Tip``() =
    let msg = Tip (Array.create 100 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://Tip.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Tip.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Tip size fits stream ``() =
    let tip:Tip =
        Array.create 100 123uy

    let messageSize = Tip.getMessageSize tip

    let stream =
        Stream.create messageSize
        |> Tip.write tip

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv NewBlock``() =
    let msg = NewBlock (Array.create 100 123uy)

    use server = Socket.dealer ()
    Socket.bind server "inproc://NewBlock.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://NewBlock.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``NewBlock size fits stream ``() =
    let newblock:NewBlock =
        Array.create 100 123uy

    let messageSize = NewBlock.getMessageSize newblock

    let stream =
        Stream.create messageSize
        |> NewBlock.write newblock

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv UnknownPeer``() =
    let msg = UnknownPeer

    use server = Socket.dealer ()
    Socket.bind server "inproc://UnknownPeer.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://UnknownPeer.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)


[<Test>]
let ``send and recv UnknownMessage``() =
    let msg = UnknownMessage 123uy

    use server = Socket.dealer ()
    Socket.bind server "inproc://UnknownMessage.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://UnknownMessage.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``UnknownMessage size fits stream ``() =
    let unknownmessage:UnknownMessage =
        123uy

    let messageSize = UnknownMessage.getMessageSize unknownmessage

    let stream =
        Stream.create messageSize
        |> UnknownMessage.write unknownmessage

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``malformed message return None``() =
    use server = Socket.dealer ()
    Socket.bind server "inproc://NetworkMessage.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://NetworkMessage.test"

    Frame.send server "hello world"B

    let msg = Network.Message.recv client
    msg |> should equal None
 