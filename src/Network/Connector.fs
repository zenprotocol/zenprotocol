module Network.Connector

open Network;
open Network.Transport;

type T = {
    maxConnections: int;
    connected: int;
    connecting: Set<string>;
}

let create maxConnection = {maxConnections=maxConnection;connected=0;connecting=Set.empty}

let connected connector address = 
    let connecting = Set.remove address connector.connecting
    let connected = connector.connected + 1
    
    {connector with connecting=connecting;connected=connected}

let disconnected connector address = 
    let connecting,connected = 
        if Set.contains address connector.connecting then
            (Set.remove address connector.connecting), connector.connected
        else connector.connecting, (connector.connected - 1)
        
    {connector with connecting=connecting;connected=connected}
    
let connect transport addressBook connector = 
    let connectionsToOpen = connector.maxConnections - connector.connected - (Set.count connector.connecting)

    let addresses = AddressBook.take addressBook connector.connecting connectionsToOpen
    
    // Connect to new addresses
    Seq.iter (Transport.connect transport) addresses
    
    // Add the new addresses to the connecting set
    let connecting = Seq.fold (fun set address -> Set.add address set) connector.connecting addresses
    
    {connector with connecting = connecting}