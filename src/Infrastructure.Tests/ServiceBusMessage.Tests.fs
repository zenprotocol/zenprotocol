module Infrastructure.ServiceBusMessage.Tests

open NUnit.Framework
open FsUnit
open FsNetMQ
open Infrastructure.ServiceBusMessage


[<Test>]
let ``send and recv Register``() =
    let msg = Register "Life is short but Now lasts for ever"

    use server = Socket.dealer ()
    Socket.bind server "inproc://Register.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Register.test"

    Infrastructure.ServiceBusMessage.send server msg

    let msg' = Infrastructure.ServiceBusMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Register size fits stream ``() =
    let register:Register =
        "Life is short but Now lasts for ever"

    let messageSize = Register.getMessageSize register

    let stream =
        Stream.create messageSize
        |> Register.write register

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Command``() =
    let msg = Command {
        service = "Life is short but Now lasts for ever";
        payload = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Command.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Command.test"

    Infrastructure.ServiceBusMessage.send server msg

    let msg' = Infrastructure.ServiceBusMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Command size fits stream ``() =
    let command:Command = {
        service = "Life is short but Now lasts for ever";
        payload = "Captcha Diem"B;
    }

    let messageSize = Command.getMessageSize command

    let stream =
        Stream.create messageSize
        |> Command.write command

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv RelayCommand``() =
    let msg = RelayCommand ("Captcha Diem"B)

    use server = Socket.dealer ()
    Socket.bind server "inproc://RelayCommand.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://RelayCommand.test"

    Infrastructure.ServiceBusMessage.send server msg

    let msg' = Infrastructure.ServiceBusMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``RelayCommand size fits stream ``() =
    let relaycommand:RelayCommand =
        "Captcha Diem"B

    let messageSize = RelayCommand.getMessageSize relaycommand

    let stream =
        Stream.create messageSize
        |> RelayCommand.write relaycommand

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Request``() =
    let msg = Request {
        requestId = System.Guid.Parse("DED9D22A-FCE1-488B-8761-84752ACC113E");
        service = "Life is short but Now lasts for ever";
        payload = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Request.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Request.test"

    Infrastructure.ServiceBusMessage.send server msg

    let msg' = Infrastructure.ServiceBusMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Request size fits stream ``() =
    let request:Request = {
        requestId = System.Guid.Parse("DED9D22A-FCE1-488B-8761-84752ACC113E");
        service = "Life is short but Now lasts for ever";
        payload = "Captcha Diem"B;
    }

    let messageSize = Request.getMessageSize request

    let stream =
        Stream.create messageSize
        |> Request.write request

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv RelayRequest``() =
    let msg = RelayRequest {
        requestId = System.Guid.Parse("DED9D22A-FCE1-488B-8761-84752ACC113E");
        sender = "Captcha Diem"B;
        payload = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://RelayRequest.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://RelayRequest.test"

    Infrastructure.ServiceBusMessage.send server msg

    let msg' = Infrastructure.ServiceBusMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``RelayRequest size fits stream ``() =
    let relayrequest:RelayRequest = {
        requestId = System.Guid.Parse("DED9D22A-FCE1-488B-8761-84752ACC113E");
        sender = "Captcha Diem"B;
        payload = "Captcha Diem"B;
    }

    let messageSize = RelayRequest.getMessageSize relayrequest

    let stream =
        Stream.create messageSize
        |> RelayRequest.write relayrequest

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Response``() =
    let msg = Response {
        requestId = System.Guid.Parse("DED9D22A-FCE1-488B-8761-84752ACC113E");
        sender = "Captcha Diem"B;
        payload = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://Response.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Response.test"

    Infrastructure.ServiceBusMessage.send server msg

    let msg' = Infrastructure.ServiceBusMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``Response size fits stream ``() =
    let response:Response = {
        requestId = System.Guid.Parse("DED9D22A-FCE1-488B-8761-84752ACC113E");
        sender = "Captcha Diem"B;
        payload = "Captcha Diem"B;
    }

    let messageSize = Response.getMessageSize response

    let stream =
        Stream.create messageSize
        |> Response.write response

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv RelayResponse``() =
    let msg = RelayResponse {
        requestId = System.Guid.Parse("DED9D22A-FCE1-488B-8761-84752ACC113E");
        payload = "Captcha Diem"B;
    }

    use server = Socket.dealer ()
    Socket.bind server "inproc://RelayResponse.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://RelayResponse.test"

    Infrastructure.ServiceBusMessage.send server msg

    let msg' = Infrastructure.ServiceBusMessage.recv client

    msg' |> should equal (Some msg)

[<Test>]
let ``RelayResponse size fits stream ``() =
    let relayresponse:RelayResponse = {
        requestId = System.Guid.Parse("DED9D22A-FCE1-488B-8761-84752ACC113E");
        payload = "Captcha Diem"B;
    }

    let messageSize = RelayResponse.getMessageSize relayresponse

    let stream =
        Stream.create messageSize
        |> RelayResponse.write relayresponse

    let offset = Stream.getOffset stream

    messageSize |> should equal offset

[<Test>]
let ``send and recv Ack``() =
    let msg = Ack

    use server = Socket.dealer ()
    Socket.bind server "inproc://Ack.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://Ack.test"

    Infrastructure.ServiceBusMessage.send server msg

    let msg' = Infrastructure.ServiceBusMessage.recv client

    msg' |> should equal (Some msg)


[<Test>]
let ``malformed message return None``() =
    use server = Socket.dealer ()
    Socket.bind server "inproc://ServiceBusMessage.test"

    use client = Socket.dealer ()
    Socket.connect client "inproc://ServiceBusMessage.test"

    Frame.send server "hello world"B

    let msg = Infrastructure.ServiceBusMessage.recv client
    msg |> should equal None

