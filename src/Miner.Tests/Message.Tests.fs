module Miner.Tests.MessageTests

open NUnit.Framework
open FsUnit
open FsNetMQ
open Miner.Message


[<Test>]
let ``send and recv NewTemplate``() =
    let msg = NewTemplate ("Captcha Diem"B)

    use server = Socket.dealer ()
    Socket.bind server "inproc://NewTemplate.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://NewTemplate.test"

    Miner.Message.send server msg

    let msg' = Miner.Message.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``NewTemplate size fits stream ``() =
    let newtemplate:NewTemplate =
        "Captcha Diem"B

    let messageSize = NewTemplate.getMessageSize newtemplate

    let stream =
        Stream.create messageSize
        |> NewTemplate.write newtemplate

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Stop``() =
    let msg = Stop

    use server = Socket.dealer ()
    Socket.bind server "inproc://Stop.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Stop.test"

    Miner.Message.send server msg

    let msg' = Miner.Message.recv client

    msg' |> should equal (Some msg)


[<Test>]
let ``send and recv Exit``() =
    let msg = Exit

    use server = Socket.dealer ()
    Socket.bind server "inproc://Exit.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Exit.test"

    Miner.Message.send server msg

    let msg' = Miner.Message.recv client

    msg' |> should equal (Some msg)


[<Test>]
let ``malformed message return None``() =
    use server = Socket.dealer ()
    Socket.bind server "inproc://Message.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Message.test"

    Frame.send server "hello world"B

    let msg = Miner.Message.recv client
    msg |> should equal None
 