module Network.Connector

open Network
open Network;
open Network.Transport;

type T = {
    maxConnections: int;
    connections: Set<string>;   
    failing: Map<string, int>;
}

[<LiteralAttribute>]
let MaxAttemps = 10

let create maxConnection = {maxConnections=maxConnection; connections = Set.empty; failing=Map.empty}

let connected connector address =
    let failed = Map.remove address connector.failing     
     
    {connector with failing = failed}
        
let disconnected connector address =  
    let connections = Set.remove address connector.connections
    
    let failing = 
        match Map.tryFind address connector.failing with
        | Some attempts ->                      
            Map.add address (attempts + 1) connector.failing
        | None ->
            Map.add address 1 connector.failing             
                
    {connector with connections = connections; failing=failing}
    
let connect transport addressBook connector =
        
    let connectInternal transport addressBook connector exclude  =
        let connectionsToOpen = connector.maxConnections - (Set.count connector.connections)
            
        let addresses = AddressBook.take addressBook exclude connectionsToOpen
        
        // Connect to new addresses
        Seq.iter (Transport.connect transport) addresses
        
        // Add the new addresses to the connecting set
        let connections = Seq.fold (fun set address -> Set.add address set) connector.connections addresses
        
        {connector with connections = connections}     
        
    let failing = 
        connector.failing          
        |> Map.toSeq 
        |> Seq.map fst 
        |> Set.ofSeq
        
    let failed = 
        connector.failing 
        |> Map.filter( fun _ attempts -> attempts = MaxAttemps) 
        |> Map.toSeq 
        |> Seq.map fst 
        |> Set.ofSeq                        
        
    // Try first without failed one
    let connector = connectInternal transport addressBook connector (Set.union connector.connections failing) 
    connectInternal transport addressBook connector (Set.union connector.connections failed) 