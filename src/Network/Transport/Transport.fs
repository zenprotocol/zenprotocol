module Network.Transport.Transport

open FSharp.Control.Reactive
open FsNetMQ
open Network
open Infrastructure
open Network.Transport
open Network.Transport.Peer

// Random number for sending addresses
let random = (new System.Random()).Next()
let timerInterval = 1 * 1000<milliseconds>

type T =
    {
        inproc: Socket.T;
        actor: Actor.T;
    }
    interface System.IDisposable with
        member x.Dispose() =
           Disposables.dispose x.actor
           Disposables.dispose x.inproc

type Peers = Map<RoutingId.T,Peer.Peer>

let cleanDeadPeers inproc (peers:Peers) =

    Map.filter (fun _ -> Peer.isDead) peers
    |> Map.iter (fun _ peer ->
            match Peer.getAddress peer with
            | Some address ->
                InProcMessage.send inproc (InProcMessage.Disconnected address)
            | None -> ())

    Map.filter (fun _ -> Peer.isDead >> not) peers

let private publishMessage socket inproc peers msg =
    let peers = Map.map (fun _ peer ->
        match Peer.isActive peer with
        | true ->  Peer.send socket peer msg
        | false -> peer) peers

    cleanDeadPeers inproc peers

let private sendToPeer socket inproc peers routingId msg =
    let peers =
        match Map.tryFind routingId peers with
        | Some peer ->
            let peer = Peer.send socket peer msg
            Map.add routingId peer peers
        | None -> peers

    cleanDeadPeers inproc peers

let private sendToPeers socket inproc peers routingIds msg =
    let peers =
        Map.map (fun routingId peer ->
            if Seq.contains routingId routingIds then
                Peer.send socket peer msg
            else peer) peers

    cleanDeadPeers inproc peers

let private sendToNextPeer socket inproc peers msg =
    // TODO: load balance peers

    let activePeers = peers |> Map.toSeq |> Seq.filter (fun (_,peer) -> Peer.isActive peer)

    if not <| Seq.isEmpty activePeers then
        let random =
            ((new System.Random()).Next()) % (Seq.length activePeers)

        let routingId = Seq.item random activePeers |> fst

        sendToPeer socket inproc peers routingId msg
    else
        peers

let private handleTimer socket inproc (peers:Peers) =
    let peers = Map.map (fun _ peer -> Peer.handleTick socket peer) peers

    cleanDeadPeers inproc peers

let private handleMessage socket inproc routingId msg (peers:Peers) =
    let next msg = InProcMessage.send inproc msg

    let peer =
        match Map.tryFind routingId peers with
        | Some peer -> Peer.handleMessage socket next peer msg
        | None -> Peer.newPeer socket next routingId msg

    let peers = Map.add routingId peer peers

    cleanDeadPeers inproc peers

let private handleInprocMessage socket inproc msg (peers:Peers) =
    match msg with
    | None -> failwith "invalid inproc msg"
    | Some msg ->
        match msg with
        | InProcMessage.Connect address ->
            let peer = Peer.connect socket address
            let routingId = Peer.routingId peer

            Map.add routingId peer peers
        | InProcMessage.Transaction tx ->
            publishMessage socket inproc peers (Message.Transaction tx)
        | InProcMessage.Address address ->
            // we only want to publish to 2 peers and want to pick them randomly
            let today = System.DateTime.Now.Day

            let selectedPeers =
                Map.toSeq peers
                |> Seq.map (fun (key,_)-> key,hash (key, today, random))
                |> Seq.sortBy (fun (_, h) -> h)
                |> Seq.take 2
                |> Seq.map fst

            sendToPeers socket inproc peers selectedPeers (Message.Address address)
        | InProcMessage.SendAddress {address=address;peerId=peerId} ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId (Message.Address address)
        | InProcMessage.GetAddresses peerId ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId Message.GetAddresses
        | InProcMessage.SendAddresses {addresses=addresses;peerId=peerId} ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId (Message.Addresses addresses)
        | InProcMessage.GetMemPool peerId ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId Message.GetMemPool
        | InProcMessage.MemPool {peerId=peerId;txs=txs} ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId (Message.MemPool txs)
        | InProcMessage.GetTransaction {peerId=peerId; txHash=txHash} ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId (Message.GetTransaction txHash)
        | InProcMessage.SendTransaction {peerId=peerId;tx=tx} ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId (Message.Transaction tx)
        | InProcMessage.PublishBlock blockHeader ->
            publishMessage socket inproc peers (Message.NewBlock blockHeader)
        | InProcMessage.GetTip peerId ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId Message.GetTip
        | InProcMessage.SendBlock {peerId=peerId; block=block} ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId (Message.Block block)
        | InProcMessage.GetBlock blockHash ->
            sendToNextPeer socket inproc peers (Message.GetBlock blockHash)
        | InProcMessage.GetNewBlock {peerId=peerId; blockHash=blockHash} ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId (Message.GetBlock blockHash)
        | InProcMessage.SendTip {peerId=peerId; blockHeader=blockHeader} ->
            let routingId = RoutingId.fromBytes peerId
            sendToPeer socket inproc peers routingId (Message.Tip blockHeader)
        | InProcMessage.PublishAddressToAll address ->
            publishMessage socket inproc peers (Message.Address address)
        | InProcMessage.GetHeaders request ->
            let routingId = RoutingId.fromBytes request.peerId
            sendToPeer socket inproc peers routingId (Message.GetHeaders {blockHash=request.blockHash;numberOfHeaders=request.numberOfHeaders})
        | InProcMessage.SendHeaders request ->
            let routingId = RoutingId.fromBytes request.peerId
            sendToPeer socket inproc peers routingId (Message.Headers request.headers)
        | msg -> failwithf "unexpected inproc msg %A" msg

let private onError error =
    Log.error "Unhandled exception from peer actor %A" error
    System.Environment.FailFast(sprintf "Unhandled exception peer actor" , error)

let connect transport address  =
    InProcMessage.send transport.inproc (InProcMessage.Connect address)

let publishTransaction transport tx =
    InProcMessage.send transport.inproc (InProcMessage.Transaction tx)

let sendAddress transport peerId address =
    InProcMessage.send transport.inproc (InProcMessage.SendAddress {address=address;peerId=peerId})

let getAddresses transport peerId =
    InProcMessage.send transport.inproc (InProcMessage.GetAddresses peerId)

let sendAddresses transport peerId addresses=
    InProcMessage.send transport.inproc (InProcMessage.SendAddresses {addresses=addresses;peerId=peerId})

let publishAddress transport address =
    InProcMessage.send transport.inproc (InProcMessage.Address address)

let getMemPool transport peerId =
    InProcMessage.send transport.inproc (InProcMessage.GetMemPool peerId)

let sendMemPool transport peerId txHashes =
    InProcMessage.send transport.inproc (InProcMessage.MemPool {peerId=peerId;txs = txHashes})

let getTransaction transport peerId txHash  =
    InProcMessage.send transport.inproc (InProcMessage.GetTransaction {peerId=peerId;txHash=txHash})

let sendTransaction transport peerId tx =
    InProcMessage.send transport.inproc (InProcMessage.SendTransaction {peerId=peerId;tx=tx})

let sendBlock transport peerId block =
    InProcMessage.send transport.inproc (InProcMessage.SendBlock {peerId=peerId;block=block})

let getTip transport peerId =
    InProcMessage.send transport.inproc (InProcMessage.GetTip peerId)

let publisNewBlock transport blockHeader =
    InProcMessage.send transport.inproc (InProcMessage.PublishBlock blockHeader)

let getNewBlock transport peerId blockHash =
    InProcMessage.send transport.inproc (InProcMessage.GetNewBlock {peerId=peerId;blockHash=blockHash})

let sendTip transport peerId blockHeader =
    InProcMessage.send transport.inproc (InProcMessage.SendTip {peerId=peerId;blockHeader=blockHeader})

let getBlock transport blockHash =
    InProcMessage.send transport.inproc (InProcMessage.GetBlock blockHash)

let publishAddressToAll transport ipAddress =
    InProcMessage.PublishAddressToAll ipAddress
    |> InProcMessage.send transport.inproc

let getHeaders transport peerId blockHash numberOfHeaders =
    InProcMessage.GetHeaders {peerId=peerId;blockHash=blockHash;numberOfHeaders=numberOfHeaders}
    |> InProcMessage.send transport.inproc

let sendHeaders transport peerId headers =
    InProcMessage.SendHeaders {peerId=peerId;headers=headers}
    |> InProcMessage.send transport.inproc

let recv transport =
    match InProcMessage.recv transport.inproc with
    | Some msg -> msg
    | None -> failwith "invalid inproc msg"

let tryRecv transport timeout =
    match InProcMessage.tryRecv transport.inproc timeout with
    | Some msg -> Some msg
    | None -> None

let addToPoller poller transport =
    Poller.addSocket poller transport.inproc

let create listen bind =
    let user,inproc = Pair.createPairs ()

    let actor = FsNetMQ.Actor.create (fun shim ->
        use poller = Poller.create ()
        use observer = Poller.registerEndMessage poller shim

        use socket = Socket.peer ()
        if listen then
            Log.info "Listening on %s" bind

            let address = sprintf "tcp://%s" bind
            Socket.bind socket address

        let socketObservable =
            Poller.addSocket poller socket
            |> Observable.map (fun _ ->
                let routingId = RoutingId.get socket
                let msg =  Message.recv socket
                handleMessage socket inproc routingId msg)

        let timer = Timer.create timerInterval
        let timerObservable =
                Poller.addTimer poller timer
                |> Observable.map (fun _ -> handleTimer socket inproc)

        let inprocObservable =
            Poller.addSocket poller inproc
            |> Observable.map (fun _ ->
                let msg = InProcMessage.recv inproc
                handleInprocMessage socket inproc msg)

        use observer =
            Observable.merge socketObservable timerObservable
            |> Observable.merge inprocObservable
            |> Observable.scanInit Map.empty (fun state handler -> handler state)
            |> Observable.subscribeWithError ignore onError

        Actor.signal shim
        Poller.run poller

        Disposables.dispose inproc
    )

    {actor=actor;inproc = user}