module Network.Main

open System.Net
open FSharp.Control
open Network
open FsNetMQ
open Infrastructure
open Messaging.Services.Network
open Messaging.Events
open Consensus
open Infrastructure.ServiceBus.Agent
open Messaging
open Messaging.Services
open Network
open Network.Message
open Network.Transport
open Serialization
open Consensus.Chain
open Logary.Message

type State = Connector.T * AddressBook.T * string option

let maxConnections = 10

let eventHandler transport event (connector,addressBook, ownAddress) =
    match event with
    | Event.TransactionAddedToMemPool (txHash, tx) ->
        let bytes = Transaction.serialize Full tx

        Transport.publishTransaction transport bytes
        connector,addressBook,ownAddress
    | _ -> connector,addressBook,ownAddress

let transportHandler transport seeds client msg (connector,addressBook,ownAddress) =
    let requestAddresses peerId =
         if not (AddressBook.haveEnoughAddresses addressBook) then
            Transport.getAddresses transport peerId

    let requestMemPool = Transport.getMemPool transport
    let requestTip = Transport.getTip transport

    match msg with
    | InProcMessage.Transaction msg ->
        match Transaction.deserialize Full msg with
        | Some tx ->
            Services.Blockchain.validateTransaction client tx
            connector,addressBook,ownAddress
        | None ->
            //TODO: log non-deserializable transaction
            connector,addressBook,ownAddress
    | InProcMessage.Connected {address=address;peerId=peerId} ->

        // We just connected to a remote peer, lets send him our address
        Option.iter (fun address ->
            Transport.sendAddress transport peerId address) ownAddress

        // Request addresses and mempool
        requestAddresses peerId
        requestMemPool peerId
        requestTip peerId

        // TODO: we might want to publish an address only once?
        if (AddressBook.contains addressBook address) then
            Transport.publishAddress transport address

        (Connector.connected connector address),addressBook,ownAddress
    | InProcMessage.Accepted peerId ->

        // Request addresses and mempool
        requestAddresses peerId
        requestMemPool peerId
        requestTip peerId

        connector,addressBook,ownAddress
    | InProcMessage.Disconnected address ->
        let connector = Connector.disconnected connector address
        (Connector.connect transport addressBook connector),addressBook,ownAddress
    | InProcMessage.Address address ->
        match Endpoint.isValid address with
        | false ->
            eventX "Received invalid address from peer {address}"
            >> setField "address" address
            |> Log.warning
            connector, addressBook,ownAddress // TODO: we should punish the sending node
        | true ->
            let handleAddress () =
                eventX "Received new address {address}"
                >> setField "address" address
                |> Log.warning

                if not (AddressBook.contains addressBook address) && not (Seq.contains address seeds) then
                    let addressBook = AddressBook.add addressBook address

                    // We might need more peers so lets try to connect to the new peer
                    (Connector.connect transport addressBook connector), addressBook,ownAddress
                else connector,addressBook,ownAddress

            match ownAddress with
            | None -> handleAddress ()
            | Some ownAddress when ownAddress <> address -> handleAddress ()
            | _ ->
                // Own address, do nothing
                connector, addressBook,ownAddress
    | InProcMessage.GetAddresses peerId ->
        Transport.sendAddresses transport peerId (AddressBook.getValidAddresses addressBook)
        connector, addressBook,ownAddress
    | InProcMessage.Addresses addresses ->
        match List.forall Endpoint.isValid addresses with
        | false ->
            eventX "Received invalid addresses from peer"
            |> Log.warning

            connector, addressBook,ownAddress // TODO: we should punish the sending node
        | true ->
            // Filter own address
            let addresses =
                match ownAddress with
                | Some ownAddress -> List.filter (fun a -> a <> ownAddress) addresses
                | None -> addresses

            let addressBook = AddressBook.addList addressBook addresses
            let connector = Connector.connect transport addressBook connector

            connector, addressBook,ownAddress
    | InProcMessage.GetMemPool peerId ->
        Blockchain.requestMemPool client peerId

        connector, addressBook,ownAddress
    | InProcMessage.MemPool {peerId=peerId;txs=bytes} ->
        // Check if valid hashses array
        // TODO: punish sending node if not
        if (Array.length bytes) % Hash.Length = 0 then
            let txHashes =
                Array.chunkBySize Hash.Length bytes
                |> Seq.ofArray
                |> Seq.map Hash.fromBytes
                |> Seq.choose id // We know all should pass as we already checked the size
                |> Seq.toList

            Blockchain.handleMemPool client peerId txHashes

        connector, addressBook,ownAddress
    | InProcMessage.GetTransaction {peerId=peerId;txHash=txHash} ->
        match Hash.fromBytes txHash with
        | Some txHash ->
            Blockchain.requestTransaction client peerId txHash
        | None ->
            // TODO: we might want to punish the sending node
            ()

        connector, addressBook,ownAddress
    | InProcMessage.BlockRequest {peerId=peerId;blockHash=blockHash} ->
        match Hash.fromBytes blockHash with
        | Some blockHash ->
            Blockchain.requestBlock client peerId blockHash
        | None -> ()

        connector, addressBook,ownAddress
    | InProcMessage.GetTip peerId ->
        Blockchain.requestTip client peerId
        connector, addressBook,ownAddress
    | InProcMessage.Block {peerId=peerId;block=block} ->
        match Block.deserialize block with
        | Some block ->
            Blockchain.validateBlock client peerId block
            connector,addressBook,ownAddress
        | None ->
            //TODO: log non-deserializable block
            connector,addressBook,ownAddress
    | InProcMessage.Tip {peerId=peerId;blockHeader=blockHeader} ->
        match Header.deserialize blockHeader with
        | Some blockHeader ->
            Blockchain.handleTip client peerId blockHeader
            connector,addressBook,ownAddress
        | None ->
            //TODO: log non-deserializable blockheader
            connector,addressBook,ownAddress
    | InProcMessage.NewBlock {peerId=peerId;blockHeader=blockHeader} ->
        match Header.deserialize blockHeader with
        | Some blockHeader ->
            Blockchain.validateNewBlockHeader client peerId blockHeader
            connector,addressBook,ownAddress
        | None ->
            //TODO: log non-deserializable blockheader
            connector,addressBook,ownAddress
    | InProcMessage.HeadersRequest {peerId=peerId;from=from;endHash=endHash} ->
        let from =
            Array.chunkBySize Hash.Length from
            |> Seq.choose Hash.fromBytes
            |> List.ofSeq

        match List.isEmpty from, Hash.fromBytes endHash with
        | false, Some endHash -> Blockchain.requestHeaders client peerId from endHash
        | _ -> ()

        connector,addressBook,ownAddress
    | InProcMessage.Headers {peerId=peerId;headers=headers} ->
        let headers =
            Array.chunkBySize Serialization.SerializedHeaderSize headers
            |> Seq.choose Header.deserialize
            |> List.ofSeq

        Blockchain.handleHeaders client peerId headers

        connector,addressBook,ownAddress
    | _ ->
        // TODO: log unknown message
        connector, addressBook,ownAddress

let commandHandler transport command (state:State) =
    match command with
    | Command.SendMemPool (peerId, txHashes) ->
        let bytes =
            List.map Hash.bytes txHashes
            |> Array.concat
        Transport.sendMemPool transport peerId bytes
        state
    | Command.GetTransaction (peerId, txHash) ->
        Transport.getTransaction transport peerId (Hash.bytes txHash)
        state
    | Command.SendTransaction (peerId, tx) ->
        let bytes = Transaction.serialize Full tx
        Transport.sendTransaction transport peerId bytes
        state
    | Command.SendBlock (peerId, block) ->
        let bytes = Block.serialize block
        Transport.sendBlock transport peerId bytes
        state
    | Command.SendTip (peerId,blockHeader) ->
        let bytes = Header.serialize blockHeader
        Transport.sendTip transport peerId bytes
        state
    | Command.GetBlock blockHash ->
        Transport.getBlock transport (Hash.bytes blockHash)
        state
    | Command.GetBlockFrom (peerId,blockHash) ->
        Transport.getBlockFrom transport peerId (Hash.bytes blockHash)
        state
    | Command.PublishBlock blockHeader ->
        let bytes = Header.serialize blockHeader
        Transport.publisNewBlock transport bytes
        state
    | Command.GetHeaders (peerId,from,endHash) ->
        let from =
            List.map Hash.bytes from
            |> Array.concat

        Transport.getHeaders transport peerId from (Hash.bytes endHash)
        state
    | Command.SendHeaders (peerId, headers) ->
        let headers =
            List.map Header.serialize headers
            |> Array.ofList
            |> Array.concat

        Transport.sendHeaders transport peerId headers
        state
    | Command.DisconnectPeer peerId ->
        Transport.disconnectPeer transport peerId
        state
    | Command.GetTipFromAllPeers ->
        Transport.getTipFromAllPeers transport
        state

let handleIpAddressFound bind transport ipAddress (connector,addressBook,ownAddress) =
    let port = Endpoint.getPort bind
    let address = sprintf "%s:%d" ipAddress port
    let ownAddress = Some address

    Transport.publishAddressToAll transport address

    connector,addressBook,ownAddress

let requestHandler (requestId:RequestId) request (state:State) =
    match request with
    | GetConnectionCount ->
        let (connector,_,_) = state

        Connector.countConnected connector
        |> uint32
        |> requestId.reply

        state

let main busName chainParams externalIp listen bind seeds =
    Actor.create<Command, Request, Event, State> busName serviceName (fun poller sbObservable ebObservable ->
        let transport = Transport.create listen bind chainParams.networkId

        let addressBook = AddressBook.empty

        let ownAddress =
            if not (System.String.IsNullOrEmpty externalIp) && listen then
                let port = Endpoint.getPort bind

                eventX "External IP is {ip}"
                >> setField "ip" externalIp
                |> Log.info
                Some (sprintf "%s:%d" externalIp port)
            else None

        let connector =
            Connector.create seeds maxConnections
            |> Connector.connect transport addressBook

        let client = ServiceBus.Client.create busName

        let sbObservable =
            sbObservable
            |> Observable.map (fun message ->
                match message with
                | ServiceBus.Agent.Command c -> commandHandler transport c
                | ServiceBus.Agent.Request (requestId, r) -> requestHandler requestId r)

        let ebObservable =
            ebObservable
            |> Observable.map (eventHandler transport)

        let transportObservable =
            Transport.addToPoller poller transport
            |> Observable.map (fun _ -> Transport.recv transport)
            |> Observable.map (transportHandler transport seeds client)

        let discoverIpObservable, discoverIpDisposable =
            if Option.isNone ownAddress && listen then
                let discoverIp = DiscoverIP.create ()
                let observable =
                    DiscoverIP.addToPoller poller discoverIp
                    |> Observable.map (handleIpAddressFound bind transport)

                observable, Disposables.toDisposable discoverIp
            else
                FSharp.Control.Reactive.Observable.empty, Disposables.empty

        let observable =
            Observable.merge sbObservable ebObservable
            |> Observable.merge transportObservable
            |> Observable.merge discoverIpObservable
            |> Observable.scan (fun state handler -> handler state) (connector,addressBook,ownAddress)

        Disposables.fromList [Disposables.toDisposable transport;discoverIpDisposable] ,observable
    )

