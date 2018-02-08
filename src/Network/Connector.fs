module Network.Connector

open Network
open Network.Transport
open System
open Infrastructure

type ConnectionStatus = 
    | Connected
    | Connecting of attempts:int
    | Failed of attempts:int
    | Suspended of DateTime

type T = {
    seeds: List<string>    
    connections: Map<string,ConnectionStatus>        
    maxConnections: int;    
}

[<LiteralAttribute>]
let MaxAttemps = 10

let create seeds maxConnection = 
    {maxConnections=maxConnection; connections = Map.empty;seeds=Seq.toList seeds}

let connected connector address =
    let connections = Map.add address Connected connector.connections     
     
    {connector with connections = connections}
        
let disconnected connector address =  

    let connections = 
        match Map.tryFind address connector.connections with
        | Some Connected ->
            Map.remove address connector.connections        
        | Some (Connecting attempts) ->
            if attempts = MaxAttemps then
                Log.info "address %s was suspended due to too many failed attempts" address
                Map.add address (Suspended DateTime.UtcNow) connector.connections
            else         
                Map.add address (Failed attempts) connector.connections
        | _ -> failwith "expecting connected of connecting address"
                
    {connector with connections = connections;}
    
let private countConnectingOrConnected connector = 
    Map.filter (fun _ value -> 
        match value with
        | Connecting _ 
        | Connected -> true
        | _ -> false) connector.connections
    |> Map.count    
    
let connect transport addressBook connector =
        
    let connectInternal transport addressBook connector exclude =        
        let connectionsToOpen = connector.maxConnections - (countConnectingOrConnected connector)                                
            
        let exclude = exclude |> Map.toSeq |> Seq.map fst |> Set.ofSeq                        
        let addresses = AddressBook.take addressBook exclude connectionsToOpen
        
        // Connect to new addresses
        Seq.iter (Transport.connect transport) addresses
        
        // Add the new addresses to the connections set
        let connections = Seq.fold (fun connections address ->
            match Map.tryFind address connections with
            | Some (Failed attempts) ->
                Map.add address (Connecting <| attempts + 1) connections  
            | _ -> 
                Map.add address (Connecting 0) connections) connector.connections addresses
        
        {connector with connections = connections}                        
        
    let connectToSeed transport connector = 
        let connectionsToOpen = connector.maxConnections - (countConnectingOrConnected connector)       
        let addresses = 
            connector.seeds
            |> List.filter (fun address -> not <| Map.containsKey address connector.connections) 
            |> fun list ->
                if List.length list > connectionsToOpen then                
                    List.take connectionsToOpen list
                else
                    list
        
        // Connect to new addresses
        List.iter (Transport.connect transport) addresses
        
        // Add the new addresses to the connections set, we don't count attemps for seeds
        let connections = List.fold (fun connections address -> Map.add address (Connecting 0) connections) connector.connections addresses
         
        {connector with connections = connections}
                                                                       
    // Try first without failed one
    let connector = connectInternal transport addressBook connector connector.connections
           
    // Filter the excluding list to not include failed, therefore we will try to connect failed as well           
    let includeFailing connector = 
        Map.filter (fun _ status -> 
            match status with
            | Failed _ -> false
            | _ -> true) connector.connections  
     
    connectInternal transport addressBook connector (includeFailing connector)
    |> connectToSeed transport