module Network.PeersManager

open FsNetMQ
open Network
open Infrastructure

let timerInterval = 10 * 1000<milliseconds> // 10 seconds

type State = 
    | Offline
    | Satisfying
    | Active

type PeersManager = 
    {
        socket: Socket.T;
        timer:  Timer.T;
        poller: Poller.T;
        peers: Map<RoutingId.T, Peer.Peer>; 
        observable: System.IObservable<PeersManager->PeersManager>;
        observer: System.IDisposable;
        state: State;
    }
    interface System.IDisposable with
            member p.Dispose() =                
                p.observer.Dispose()             
                Poller.removeTimer p.poller p.timer                                    
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
    
let handleTimer peersManager =
    let peers = 
        Map.map (fun _ -> Peer.handleTick peersManager.socket) peersManager.peers
        |> Map.filter (fun _ peer -> not (Peer.isDead peer))
    
    {peersManager with peers = peers}
                 
let create poller listen bind seeds =    
    let socket = Socket.peer ()
    let timer = Timer.create timerInterval 
    
    if listen then 
        Log.info "Listening on %s" bind
    
        let address = sprintf "tcp://%s" bind
        Socket.bind socket address

    let peers = 
        Seq.map (fun seed ->
            let peer = Peer.connect socket (sprintf "tcp://%s" seed)
            (Peer.routingId peer), peer) seeds
        |> Map.ofSeq
    
    let timerObservable = 
        Poller.addTimer poller timer
        |> Observable.map (fun _ -> handleTimer)
    
    let socketObservable = 
        Poller.addSocket poller socket
        |> Observable.map (fun _ -> 
            let routingId = RoutingId.get socket
            let msg =  Message.recv socket
            handleMessage routingId msg)
        |> FSharp.Control.Reactive.Observable.publish
        
    let observer = FSharp.Control.Reactive.Observable.connect socketObservable
    
    let observable = Observable.merge socketObservable timerObservable
    
    {
        socket=socket; 
        poller=poller; 
        peers = peers; 
        observable=observable; 
        observer = observer; 
        timer = timer; 
        state = Offline
    }
    
let observable manager = manager.observable

let activePeers manager = 
    Map.filter (fun _ peer -> Peer.isActive peer) manager.peers
    |> Seq.length