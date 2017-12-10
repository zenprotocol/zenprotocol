module Network.Tests.PeerTests

open Xunit
open FsUnit.Xunit
open FsNetMQ
open Network

let isConnecting peer =
    match Peer.state peer with
    | Peer.Connecting _ -> true
    | _ -> false

[<Fact>]
let ``peers connecting to each other`` () =
    printfn "starting..."

    use hostSocket = Socket.peer ()
    Socket.bind hostSocket "inproc://peertopeer"
    
    use clientSocket = Socket.peer ()
    let client = Peer.connect clientSocket "inproc://peertopeer"
    
    isConnecting client |> should be True    
    
    let routingId = RoutingId.get hostSocket
    let msg = Message.recv hostSocket
    
    let host = Peer.newPeer hostSocket routingId msg
    
    Peer.state host |> should equal Peer.Active
    
    RoutingId.get clientSocket |> ignore
    let msg' = Message.recv clientSocket
    
    let next _ = ()
    
    let client' = Peer.handleMessage clientSocket next client msg'
    
    Peer.state client' |> should equal Peer.Active