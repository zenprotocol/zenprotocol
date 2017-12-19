module Network.Tests.TransportTests

open NUnit.Framework
open FsUnit
open FsNetMQ
open Network   
open Network.Tests

open TransportHelper        

[<Test>]
let ``Two peers connect to each other``() =                    
    use host = startPeer true
    use peer = startPeer false
              
    waitForAcceptedMessage host 100<milliseconds>
    waitForConnectedMessage peer 100<milliseconds>         
    
[<Test>]
let ``multiple peers connecting to host`` () =
    use host = startPeer true
    use peer = startPeer false
    
    waitForAcceptedMessage host 100<milliseconds>
    waitForConnectedMessage peer 100<milliseconds>
    
    let peer2 = startPeer false
    
    waitForAcceptedMessage host 100<milliseconds>
    waitForConnectedMessage peer2 100<milliseconds>
        
[<Test>]    
let ``peer reconnect when other peer is down``()=
    let host = startPeer true
    use peer = startPeer false
    let connector = Connector.create 1    
                           
    System.Threading.Thread.Sleep (100)
    
    waitForConnectedMessage peer 100<milliseconds>
    let connector = Connector.connected connector address
    
    // Killing host
    stopPeer host
    
    waitForDisconnectedMessage peer 4000<milliseconds>
                    
    // Starting a new host
    use host2 = startPeer true
        
    Connector.disconnected connector address
    |> Connector.connect peer addressBook
    |> ignore
    
    // We have to wait 2 seconds for the hello timeout
    waitForConnectedMessage peer 4000<milliseconds>     