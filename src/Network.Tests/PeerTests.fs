module Network.Tests.PeerTests

open NUnit.Framework
open FsUnit
open FsNetMQ
open Network
open Network.Transport

let isConnecting peer =
    match Peer.state peer with
    | Peer.Connecting _ -> true
    | _ -> false

[<Test>]
let ``peers connecting to each other`` () =
    printfn "starting..."

    use hostSocket = Socket.peer ()
    Socket.bind hostSocket "tcp://127.0.0.1:9876"
    
    use clientSocket = Socket.peer ()
    let client = Peer.connect clientSocket "127.0.0.1:9876"
    
    isConnecting client |> should be True    
    
    let routingId = RoutingId.get hostSocket
    let msg = Message.recv hostSocket
    
    let host = Peer.newPeer hostSocket (fun _ -> ()) routingId msg
    
    Peer.state host |> should equal Peer.Active
    
    RoutingId.get clientSocket |> ignore
    let msg' = Message.recv clientSocket
    
    let next _ = ()
    
    let client' = Peer.handleMessage clientSocket next client msg'
    
    Peer.state client' |> should equal Peer.Active