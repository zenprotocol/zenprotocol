module Network.PeersManager

open FsNetMQ
open Network
open Infrastructure

let maxConnections = 1
let timerInterval = 1 * 1000<milliseconds> // 10 seconds

type State = 
    | Offline
    | Online

type PeersManager = 
    {
        socket: Socket.T;
        timer:  Timer.T;
        poller: Poller.T;
        peers: Peers.T; 
        observable: System.IObservable<PeersManager->PeersManager>;
        observer: System.IDisposable;
        state: State;
        addressBook: AddressBook.T;
    }
    interface System.IDisposable with
            member p.Dispose() =                
                p.observer.Dispose()             
                Poller.removeTimer p.poller p.timer                                    
                Poller.removeSocket p.poller p.socket
                (p.socket :> System.IDisposable).Dispose()                                              
                
let checkState manager =           
    let connectingAddresses = Peers.connectingAddresses manager.peers
    let activeAndConnectingCount = 
        Peers.activeAndConnecting manager.peers
        |> Seq.length
                        
    let peers = 
        match activeAndConnectingCount < maxConnections with
        | false -> manager.peers
        | true ->
            AddressBook.take manager.addressBook connectingAddresses (maxConnections - activeAndConnectingCount)
            |> Peers.connect manager.peers manager.socket                
            
    let state = 
        match Peers.isActive peers with
        | true -> Online
        | false -> Offline
        
    // TODO: publishing events        
            
    {manager with peers = peers; state = state}
                           
         
let handleMessage routingId msg peersManager =
    let peer = 
        match Peers.tryGet peersManager.peers routingId with
        | Some peer -> Peer.handleMessage peersManager.socket peer msg
        | None -> Peer.newPeer peersManager.socket routingId msg
        
    let peers = 
        match Peer.isDead peer with
        | true -> Peers.remove peersManager.peers routingId 
        | false -> Peers.update peersManager.peers routingId peer 

    {peersManager with peers = peers}
    |> checkState
    
let handleTimer peersManager =
    let peers = Peers.run peersManager.peers (Peer.handleTick peersManager.socket)        
    
    // TODO: Any peer that is dead because of no HelloAck should be removed from address book
    // TODO: except seeders...
    
    {peersManager with peers = peers}
    |> checkState
                 
let create poller listen bind seeds =    
    let socket = Socket.peer ()
    let timer = Timer.create timerInterval 
    
    if listen then 
        Log.info "Listening on %s" bind
    
        let address = sprintf "tcp://%s" bind
        Socket.bind socket address  
    
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
    
    let addressBook = AddressBook.create (Seq.map (sprintf "tcp://%s") seeds)
    
    {
        socket=socket; 
        poller=poller; 
        peers = Peers.empty; 
        observable=observable; 
        observer = observer; 
        timer = timer; 
        state = Offline;
        addressBook = addressBook;
    }
    |> checkState
    
let observable manager = manager.observable

let countActivePeers manager = 
    Peers.activePeers manager.peers
    |> Seq.length

                             