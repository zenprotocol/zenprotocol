module Network.Tests.MessageTests

open Xunit
open FsUnit.Xunit
open FsNetMQ
open Network.Message


[<Fact>]
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

[<Fact>]
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

[<Fact>]
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

[<Fact>]
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

[<Fact>]
let ``send and recv Ping``() =
    let msg = Ping {
        nonce = 123ul;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Ping.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Ping.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Fact>]
let ``Ping size fits stream ``() =
    let ping:Ping = {
        nonce = 123ul;
    }

    let messageSize = Ping.getMessageSize ping

    let stream =
        Stream.create messageSize
        |> Ping.write ping

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Fact>]
let ``send and recv Pong``() =
    let msg = Pong {
        nonce = 123ul;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Pong.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Pong.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Fact>]
let ``Pong size fits stream ``() =
    let pong:Pong = {
        nonce = 123ul;
    }

    let messageSize = Pong.getMessageSize pong

    let stream =
        Stream.create messageSize
        |> Pong.write pong

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Fact>]
let ``send and recv Transaction``() =
    let msg = Transaction {
        tx = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Transaction.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Transaction.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Fact>]
let ``Transaction size fits stream ``() =
    let transaction:Transaction = {
        tx = "Captcha Diem"B;
    }

    let messageSize = Transaction.getMessageSize transaction

    let stream =
        Stream.create messageSize
        |> Transaction.write transaction

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Fact>]
let ``send and recv UnknownPeer``() =
    let msg = UnknownPeer {
        dummy = 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://UnknownPeer.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://UnknownPeer.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Fact>]
let ``UnknownPeer size fits stream ``() =
    let unknownpeer:UnknownPeer = {
        dummy = 123uy;
    }

    let messageSize = UnknownPeer.getMessageSize unknownpeer

    let stream =
        Stream.create messageSize
        |> UnknownPeer.write unknownpeer

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Fact>]
let ``send and recv UnknownMessage``() =
    let msg = UnknownMessage {
        messageId = 123uy;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://UnknownMessage.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://UnknownMessage.test"

    Network.Message.send server msg

    let msg' = Network.Message.recv client

    msg' |> should equal (Some msg)

[<Fact>]
let ``UnknownMessage size fits stream ``() =
    let unknownmessage:UnknownMessage = {
        messageId = 123uy;
    }

    let messageSize = UnknownMessage.getMessageSize unknownmessage

    let stream =
        Stream.create messageSize
        |> UnknownMessage.write unknownmessage

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Fact>]
let ``malformed message return None``() =
    use server = Socket.dealer ()
    Socket.bind server "inproc://NetworkMessage.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://NetworkMessage.test"

    Frame.send server "hello world"B

    let msg = Network.Message.recv client
    msg |> should equal None
 