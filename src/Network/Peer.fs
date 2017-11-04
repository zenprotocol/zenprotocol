module Network.Peer

open FsNetMQ
open Network
open Infrastructure

let networkId = 0ul;
let version = 0ul;

type State =    
    | Connecting
    | Active
    | Dead

type Peer = {
    routingId: RoutingId.T;
    state: State;
}

// TODO: check network on each message
// TODO: for each message we should minimum version

let state peer = peer.state
let isDead peer = peer.state = Dead
let isActive peer = peer.state = Active

let private withState peer state = { peer with state =state; }

let private closePeer socket peer =
    // TODO: terminate the pipe
    // TODO: disconnect the peer if we are conneting one
    
    withState peer Dead
    
let private send socket peer msg = 
    match RoutingId.trySet socket peer.routingId 0<milliseconds> with
    | RoutingId.TryResult.HostUnreachable 
    | RoutingId.TryResult.TimedOut -> closePeer socket peer
    | RoutingId.TryResult.Ok -> 
        Message.send socket msg
        peer
    
let private create routingId state = {routingId = routingId; state = state}       

let connect socket address =                    
    let routingId = Peer.connect socket address
    
    Log.info "Connecting to %s" address
    
    let peer = create routingId Connecting
    
    // TODO: use correct values for this
    send socket peer (Message.Hello {version=version; network = networkId;})    

let newPeer socket routingId msg = 
    let peer = create routingId Dead 
    
    match msg with
    | None -> 
        send socket peer (Message.UnknownMessage {messageId = 0uy})
        |> closePeer socket
    | Some msg ->
        match msg with
        | Message.Hello _ ->
            Log.info "Peer accepted"
            
            let peer = (withState peer Active)
            
            send socket peer (Message.HelloAck {version=0ul; network = networkId;})                                                 
        | _ ->            
            send socket peer (Message.UnknownPeer {dummy = 0uy;})

let handleConnectingState socket peer msg = 
    match msg with
    | None -> 
        Log.warning "Received malformed message from peer"
                        
        send socket peer (Message.UnknownMessage {messageId = 0uy})
        |> closePeer socket 
    | Some msg -> 
        match msg with 
        | Message.HelloAck _ -> 
            // TODO: check network match, save version
            
            Log.info "Connected to pear"
            
            withState peer Active        
        | _ -> 
            Log.warning "Expecting HelloAck" // TODO: print message name
            closePeer socket peer            

let handleActiveState socket peer msg =
    match msg with
    | None ->         
        Log.warning "Received malformed message from peer"
    
        send socket peer (Message.UnknownMessage {messageId = 0uy})
        |> closePeer socket
    | Some msg ->  
        match msg with 
        | Message.UnknownPeer _ ->
            
            let peer = withState peer Connecting
            
            send socket peer (Message.Hello {version=version;network=networkId})            
        | _ -> peer

let handleMessage socket peer msg =    
   match state peer with
   | Connecting -> handleConnectingState socket peer msg 
   | Active -> handleActiveState socket peer msg 
   | _ -> failwith "Dead peer should not receive any messages"

let routingId peer = peer.routingId