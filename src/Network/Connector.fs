module Network.Connector

open Network
open Network;
open Network.Transport;

type T = {
    maxConnections: int;
    connections: Set<string>;   
    failed: Set<string>;
}

let create maxConnection = {maxConnections=maxConnection; connections = Set.empty; failed=Set.empty}

let connected connector address =
    let failed = Set.remove address connector.failed     
     
    {connector with failed = failed}
        
let disconnected connector address =
    // TODO: add the address to fail, in order to try last 
 
    let connections = Set.remove address connector.connections
    let failed = Set.add address connector.failed
    
    {connector with connections = connections}
    
let connect transport addressBook connector =
        
    let connectInternal transport addressBook connector exclude  =
        let connectionsToOpen = connector.maxConnections - (Set.count connector.connections)
            
        let addresses = AddressBook.take addressBook exclude connectionsToOpen
        
        // Connect to new addresses
        Seq.iter (Transport.connect transport) addresses
        
        // Add the new addresses to the connecting set
        let connections = Seq.fold (fun set address -> Set.add address set) connector.connections addresses
        
        {connector with connections = connections}     
        
    // Try first without failed one
    let connector = connectInternal transport addressBook connector (Set.union connector.connections connector.failed) 
    connectInternal transport addressBook connector connector.connections 