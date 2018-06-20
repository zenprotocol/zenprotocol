module Network.Tests.TransportTests

open NUnit.Framework
open FsUnit
open FsNetMQ
open Network
open Network.Transport
open Network.Tests

open TransportHelper

[<Test>]
let ``Two peers connect to each other``() =
    use host = startHost ()
    use peer = startPeer ()

    waitForAcceptedMessage host 100<milliseconds>
    waitForConnectedMessage peer 100<milliseconds>

[<Test>]
let ``multiple peers connecting to host`` () =
    use host = startHost ()
    use peer = startPeer ()

    waitForAcceptedMessage host 100<milliseconds>
    waitForConnectedMessage peer 100<milliseconds>

    let peer2 = startPeer ()

    waitForAcceptedMessage host 100<milliseconds>
    waitForConnectedMessage peer2 100<milliseconds>

[<Test;IgnoreAttribute("Too long for CI")>]
let ``peer reconnect when other peer is down``()=
    let host = startHost ()
    use peer = startPeer ()
    let connector = Connector.create (seq {yield address}) 1

    System.Threading.Thread.Sleep (100)

    waitForConnectedMessage peer 100<milliseconds>
    let connector = Connector.connected connector address

    // Killing host
    stopPeer host

    waitForDisconnectedMessage peer 25000<milliseconds>

    // Starting a new host
    use host2 = startHost ()

    Connector.disconnected connector address
    |> Connector.connect peer addressBook
    |> ignore

    waitForConnectedMessage peer 25000<milliseconds>

[<Test>]
let ``relay address relay only to two peers``() =
    use host = startHost ()
    use peer1 = startPeer ()
    use peer2 = startPeer ()
    use peer3 = startPeer ()

    waitForAcceptedMessage host 100<milliseconds>
    waitForAcceptedMessage host 100<milliseconds>
    waitForAcceptedMessage host 100<milliseconds>

    waitForConnectedMessage peer1 100<milliseconds>
    waitForConnectedMessage peer2 100<milliseconds>
    waitForConnectedMessage peer3 100<milliseconds>

    Transport.publishAddress host "127.0.0.1:5555"

    let a1 = tryRecvAddress peer1 100<milliseconds>
    let a2 = tryRecvAddress peer2 100<milliseconds>
    let a3 = tryRecvAddress peer3 100<milliseconds>

    let addresses =
        seq {yield a1;yield a2;yield a3}
        |> Seq.choose id
        |> List.ofSeq

    addresses |> should equal ["127.0.0.1:5555";"127.0.0.1:5555"]

[<Test>]
let ``sending and receiving address book``() =
    let addresses = [ "127.0.0.1:6666"; "127.0.0.1:6667"]

    use host = startHost ()
    use peer = startPeer ()

    waitForAcceptedMessage host 100<milliseconds>

    let hostId = recvConnectedMessage peer 100<milliseconds>
    Transport.getAddresses peer hostId

    let peerId = recvGetAddresses host 100<milliseconds>
    Transport.sendAddresses host peerId addresses

    let addresses' = recvAddresses peer 100<milliseconds>

    addresses' |> should equal addresses



