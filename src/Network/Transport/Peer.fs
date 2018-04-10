module Network.Transport.Peer

open FsNetMQ
open Network
open Infrastructure
open Logary.Message

module RoutingId =
    let toBytes (FsNetMQ.RoutingId.RoutingId bytes) = bytes
    let fromBytes bytes = FsNetMQ.RoutingId.RoutingId bytes

let version = 0ul;

// TODO: those should come from configuration?
// For now those are configured for fast unit testing
let pingInterval = System.TimeSpan.FromSeconds(10.0)
let pingTimeout = System.TimeSpan.FromSeconds(5.0)
let helloTimeout = System.TimeSpan.FromSeconds(5.0)

type CloseReason =
    | NoPingReply
    | Unreachable
    | PipeFull
    | UnknownMessage
    | UnknownPeer
    | NoHelloAck
    | ExpectingHelloAck
    | NoPong
    | IncorrectNetwork

type State =
    | Connecting of sent:System.DateTime
    | Active
    | Dead of reason:CloseReason

type PingState =
    | NoPing of lastPong: System.DateTime
    | WaitingForPong of nonce:uint32 * pingSent:System.DateTime

type PeerMode =
    | Connector of address:string
    | Listener

type Peer = {
    mode: PeerMode
    routingId: RoutingId.T;
    state: State;
    ping: PingState;
    networkId: uint32;
}

let private random = new System.Random()
let private getNonce () =
    let bytes = Array.create 4 0uy
    random.NextBytes (bytes)
    System.BitConverter.ToUInt32(bytes,0)

let private getNow () = System.DateTime.UtcNow

// TODO: check network on each message
// TODO: for each message we should minimum version

let state peer = peer.state
let isDead peer =
    match peer.state with
    | Dead _ -> true
    | _ -> false

let isActive peer = peer.state = Active
let isConntecting peer =
    match peer.state with
    | Connecting _ -> true
    | _-> false

let getAddress peer =
    match peer.mode with
    | Listener -> None
    | Connector address -> Some address

let private withState peer state = { peer with state =state; }

let private disconnect socket peer =
    match peer.mode with
    | Listener -> peer
    | Connector address ->
        Socket.disconnect socket (sprintf "tcp://%s" address)
        peer

let private closePeer socket reason peer =
    eventX "Closing peer because of {reason}"
    >> setField "reason" (reason.ToString())
    |> Log.info

    disconnect socket peer |> ignore
    withState peer (Dead reason)

let send socket peer msg =
    match RoutingId.trySet socket peer.routingId 0<milliseconds> with
    | RoutingId.TryResult.HostUnreachable -> closePeer socket Unreachable peer
    | RoutingId.TryResult.TimedOut -> closePeer socket PipeFull peer
    | RoutingId.TryResult.Ok ->
        Message.send socket msg
        peer

let private create mode routingId networkId state =
    {mode=mode; routingId = routingId; state = state; networkId=networkId; ping = NoPing (getNow ())}

let connect socket networkId address =
    let routingId = Peer.connect socket (sprintf "tcp://%s" address)

    eventX "Connecting to {address}"
    >> setField "address" address
    |> Log.info

    let peer = create (Connector address) routingId networkId (Connecting (getNow ()))

    // TODO: use correct values for this
    send socket peer (Message.Hello {version=version; network = networkId;})

let newPeer socket networkId next routingId msg =
    let createPeer = create Listener routingId networkId

    match msg with
    | None ->
        let peer = createPeer (Dead UnknownMessage)
        send socket peer (Message.UnknownMessage 0uy)
        |> disconnect socket
    | Some msg ->
        match msg with
        | Message.Hello hello ->
            if hello.network <> networkId then
                let peer = createPeer (Dead IncorrectNetwork)
                send socket peer (Message.IncorrectNetwork)
                |> disconnect socket
            else
                eventX "Peer accepted"
                |> Log.info

                let peer = createPeer Active

                next (InProcMessage.Accepted (RoutingId.toBytes peer.routingId))

                send socket peer (Message.HelloAck {version=0ul; network = networkId;})
        | _ ->
            let peer = createPeer (Dead UnknownPeer)
            send socket peer (Message.UnknownPeer)
            |> disconnect socket

let handleConnectingState socket next peer msg =
    match msg with
    | None ->
        eventX "Received malformed message from peer"
        |> Log.warning

        send socket peer (Message.UnknownMessage 0uy)
        |> closePeer socket UnknownMessage
    | Some msg ->
        match msg with
        | Message.HelloAck helloAck ->
            if helloAck.network <> peer.networkId then
                closePeer socket IncorrectNetwork peer
            else
                // TODO: save version

                eventX "Connected to peer"
                |> Log.info

                match peer.mode with
                | Connector address ->
                    let peerId = RoutingId.toBytes peer.routingId
                    next (InProcMessage.Connected {address=address;peerId=peerId})
                | _ -> ()

                {peer with state=Active; ping=NoPing (getNow ())}
        | _ ->
            closePeer socket ExpectingHelloAck peer

let handleActiveState socket next peer msg =
    match msg with
    | None ->
        eventX "Received malformed message from peer"
        |> Log.info

        send socket peer (Message.UnknownMessage 0uy)
        |> closePeer socket UnknownMessage
    | Some msg ->
        match msg with
        | Message.UnknownPeer _ ->
            eventX "Reconnecting to peer"
            |> Log.info

            // NetMQ reconnection, just sending Hello again
            let peer = withState peer (Connecting (getNow ()))
            send socket peer (Message.Hello {version=version;network=peer.networkId})
        | Message.Ping nonce ->
            // TODO: should we check when we last answer a ping? the other peer might try to spoof us
            send socket peer (Message.Pong nonce)
        | Message.Pong nonce ->
            match peer.ping with
            | NoPing _ -> peer
            | WaitingForPong (nonce,_) ->
                match nonce = nonce with
                | true -> {peer with ping=NoPing (getNow ())}
                | false -> peer
        | Message.Transaction tx ->
            next (InProcMessage.Transaction tx)
            peer
        | Message.Address address ->
            next (InProcMessage.Address address)
            peer
        | Message.GetAddresses ->
            next (InProcMessage.GetAddresses (RoutingId.toBytes peer.routingId))
            peer
        | Message.Addresses addresses ->
            next (InProcMessage.Addresses addresses)
            peer
        | Message.GetMemPool ->
            next (InProcMessage.GetMemPool (RoutingId.toBytes peer.routingId))
            peer
        | Message.MemPool txs ->
            next (InProcMessage.MemPool {peerId=(RoutingId.toBytes peer.routingId);txs=txs})
            peer
        | Message.GetTransaction txHash ->
            next (InProcMessage.GetTransaction {peerId=(RoutingId.toBytes peer.routingId); txHash=txHash})
            peer
        | Message.GetBlock blockHash ->
            next (InProcMessage.BlockRequest {peerId=(RoutingId.toBytes peer.routingId); blockHash=blockHash})
            peer
        | Message.Block block ->
            next (InProcMessage.Block block)
            peer
        | Message.Tip blockHeader ->
            next (InProcMessage.Tip {peerId=RoutingId.toBytes peer.routingId;blockHeader=blockHeader})
            peer
        | Message.NewBlock blockHeader ->
            next (InProcMessage.NewBlock {peerId=(RoutingId.toBytes peer.routingId); blockHeader=blockHeader})
            peer
        | Message.GetTip ->
            next (InProcMessage.GetTip (RoutingId.toBytes peer.routingId))
            peer
        | Message.GetHeaders request ->
            next (InProcMessage.HeadersRequest {
                peerId=(RoutingId.toBytes peer.routingId);
                blockHash=request.blockHash;
                numberOfHeaders=request.numberOfHeaders;
            })
            peer
        | Message.Headers headers ->
            next (InProcMessage.Headers {peerId=(RoutingId.toBytes peer.routingId);headers=headers})
            peer
        | msg ->
            // TODO: unexpected msg, close peer

            peer

let handleMessage socket next peer msg =
   match state peer with
   | Connecting _ -> handleConnectingState socket next peer msg
   | Active -> handleActiveState socket next peer msg
   | _ -> failwith "Dead peer should not receive any messages"

let handleTick socket peer =
    match peer.state with
    | Active ->
        match peer.ping with
        | NoPing lastPong ->
            match (getNow ()) - lastPong > pingInterval with
            | false -> peer
            | _ ->
                let nonce = getNonce ()
                let peer = send socket peer (Message.Ping nonce)
                match peer.state with
                | Active ->
                    {peer with ping=WaitingForPong (nonce, (getNow ()))}
                | _ -> peer
        | WaitingForPong (nonce,sentTime) ->
            match (getNow ()) - sentTime > pingTimeout with
            | false -> peer
            | true -> closePeer socket NoPong peer
    | Connecting sent ->
        match (getNow ()) - sent > helloTimeout with
        | false -> peer
        | true ->
            closePeer socket NoHelloAck peer
    | _ -> failwith "Dead peer should not receive any ticks requests"

let routingId peer = peer.routingId