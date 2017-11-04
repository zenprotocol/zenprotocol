module Network.PeersManager

open FsNetMQ
open Network
open Infrastructure

type PeersManager = 
    {
        socket:Socket.T;
        poller: Poller.T;
        peers: Map<RoutingId.T, Peer.Peer>; 
        observable: System.IObservable<PeersManager->PeersManager>;
        observer: System.IDisposable;
    }
    interface System.IDisposable with
            member p.Dispose() =                
                p.observer.Dispose()                                                 
                Poller.removeSocket p.poller p.socket
                (p.socket :> System.IDisposable).Dispose()
         
let handleMessage routingId msg peersManager =
    let peer = 
        match Map.tryFind routingId peersManager.peers with
        | Some peer -> Peer.handleMessage peersManager.socket peer msg
        | None -> Peer.newPeer peersManager.socket routingId msg
        
    let peers = 
        match Peer.isDead peer with
        | true -> Map.remove routingId peersManager.peers
        | false -> Map.add routingId peer peersManager.peers

    {peersManager with peers = peers}
                 
let create poller listen bind seeds =    
    let socket = Socket.peer () 
    
    if listen then 
        Log.info "Listening on %s" bind
    
        let address = sprintf "tcp://%s" bind
        Socket.bind socket address

    let peers = 
        Seq.map (fun seed ->
            let peer = Peer.connect socket (sprintf "tcp://%s" seed)
            (Peer.routingId peer), peer) seeds
        |> Map.ofSeq          
    
    let observable = 
        Poller.addSocket poller socket
        |> Observable.map (fun _ -> 
            let routingId = RoutingId.get socket
            let msg =  Message.recv socket
            handleMessage routingId msg)
        |> FSharp.Control.Reactive.Observable.publish
        
    let observer = FSharp.Control.Reactive.Observable.connect observable                  
    
    {socket=socket; poller=poller; peers = peers; observable=observable; observer = observer}
    
let observable manager = manager.observable

let activePeers manager = 
    Map.filter (fun _ peer -> Peer.isActive peer) manager.peers
    |> Seq.length