module Network.Tests.PeerTests

open Xunit
open FsUnit.Xunit
open FsNetMQ
open Network

[<Fact>]
let ``peers connecting to each other`` () =
    use hostSocket = Socket.dealer ()
    Socket.bind hostSocket "inproc://peertopeer"
    
    use clientSocket = Socket.dealer ()        
    let client = Peer.connect clientSocket "inproc://peertopeer"
    
    Peer.state client |> should equal Peer.Connecting
    
    let routingId = Peer.recvRoutingId hostSocket
    let msg = Message.recv hostSocket
    
    let host = Peer.newPeer hostSocket routingId msg
    
    Peer.state host |> should equal Peer.Active
    
    Peer.recvRoutingId clientSocket |> ignore
    let msg' = Message.recv clientSocket
    
    let client' = Peer.handleMessage clientSocket client msg'
    
    Peer.state client' |> should equal Peer.Active  
    
      
    
    