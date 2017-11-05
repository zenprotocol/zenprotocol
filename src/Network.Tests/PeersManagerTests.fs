module PeersManagerTests

open Xunit
open FsUnit.Xunit
open FsNetMQ
open Network
open Network.Tests.PeersManagerHelper
            

[<Fact>]
let ``Two peers connect to each other``() =            
    use manager1 = startPeer true
    use manager2 = startPeer false
                           
    System.Threading.Thread.Sleep (100)
    
    1 |> should equal (getActivePeers manager1)
    1 |> should equal (getActivePeers manager2)
    
[<Fact>]    
let ``multiple peers connecting to host`` () =
    use host = startPeer true
    use peer = startPeer false
    
    System.Threading.Thread.Sleep (100)
        
    1 |> should equal (getActivePeers peer)
    1 |> should equal (getActivePeers host)
    
    let peer2 = startPeer false
    
    System.Threading.Thread.Sleep (100)
            
    1 |> should equal (getActivePeers peer)
    1 |> should equal (getActivePeers peer2)
    2 |> should equal (getActivePeers host)
    
    stopPeer peer2
    
    System.Threading.Thread.Sleep (3000)
    
    1 |> should equal (getActivePeers peer)    
    1 |> should equal (getActivePeers host)
    
[<Fact>]    
let ``peer reconnect when other peer is down``()=
    let host = startPeer true
    use peer = startPeer false
                           
    System.Threading.Thread.Sleep (100)
    
    1 |> should equal (getActivePeers peer)
    1 |> should equal (getActivePeers host)
    
    // Killing host
    stopPeer host
    
    // We have to wait alot... 4 seconds to the keep alive to recognize the host is down
    // TODO: find a way to change the interval and timeout for testing
    System.Threading.Thread.Sleep (4000)
    
    0 |> should equal (getActivePeers peer)
    
    // Starting a new host
    use host2 = startPeer true
    
    // We have to wait 2 seconds for the hello timeout
    System.Threading.Thread.Sleep (1500)
        
    1 |> should equal (getActivePeers peer)
    1 |> should equal (getActivePeers host2)

            