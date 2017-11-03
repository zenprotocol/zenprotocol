module Network.Peer

open FsNetMQ
open Network
open Infrastructure

type RoutingId = RoutingId of uint32

let networkId = 0ul;
let version = 0ul;

type State =    
    | Connecting
    | Active
    | Dead

type T = Peer of State

// TODO: check network on each message
// TODO: for each message we should minimum version

let state (Peer state) = state
let isDead (Peer state) = state = Dead

let private closePeer peer socket =
    // TODO: terminate the pipe
    // TODO: disconnect the peer if we are conneting one
    
    Peer Dead

let connect socket address =
    let address = sprintf "tcp://%s" address 
            
    // TODO: set the routing id
    Socket.connect socket address
    
    Log.info "Connecting to %s" address
    
    let peer = Peer Connecting
    
    // TODO: use correct values for this
    let hello = Message.Hello {version=version; network = networkId;}
    Message.send socket hello
    
    peer

let newPeer socket routingId msg =     
    match msg with
    | None -> 
        Message.send socket (Message.UnknownMessage {messageId = 0uy})
        Peer Dead
    | Some msg ->
        match msg with
        | Message.Hello _ ->
            let helloAck = Message.HelloAck {version=0ul; network = networkId;}
            Message.send socket helloAck
            
            Log.info "Peer accepted"
            
            Peer Active
        | _ ->
            let unknownPeer = Message.UnknownPeer {dummy = 0uy;}
            Message.send socket unknownPeer
            Peer Dead

let handleConnectingState socket peer msg = 
    match msg with
    | None -> 
        Message.send socket (Message.UnknownMessage {messageId = 0uy})
        Log.warning "Received malformed message from peer"
        closePeer peer socket
    | Some msg -> 
        match msg with 
        | Message.HelloAck _ -> 
            // TODO: check network match, save version
            
            Log.info "Connected to pear"
            
            Peer Active        
        | _ -> 
            Log.warning "Expecting HelloAck" // TODO: print message name
            // TODO: terminate the pipe
            // TODO: disconnect the peer if we are conneting one
            Peer Dead

let handleActiveState socket peer msg =
    match msg with
    | None -> 
        Message.send socket (Message.UnknownMessage {messageId = 0uy})
        
        closePeer peer socket
    | Some msg ->  
        match msg with 
        | Message.UnknownPeer _ ->
            Message.send socket (Message.Hello {version=version;network=networkId})
            Peer Connecting
        | _ -> Peer Active                                            

let handleMessage socket peer msg =    
   match state peer with
   | Connecting -> handleConnectingState socket peer msg 
   | Active -> handleActiveState socket peer msg 
   | _ -> Peer Dead

let routingId peer = RoutingId 0ul

let recvRoutingId socket = RoutingId 0ul