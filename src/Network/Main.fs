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
open Network
open Network

type State = Connector.T * TransactionPublisher.T * string option

let maxConnections = 10

let eventHandler transport event (connector,publisher,ownAddress) =
    match event with
    | Event.TransactionAddedToMemPool (txHash, tx) ->
        let publisher = TransactionPublisher.add txHash publisher
        connector,publisher,ownAddress
    | _ -> connector,publisher,ownAddress

let transportHandler transport seeds client addressBook now msg (connector,publisher,ownAddress) =
    let requestAddresses peerId =
         if not (AddressBook.haveEnoughAddresses addressBook) then
            Transport.getAddresses transport peerId

    let requestMemPool = Transport.getMemPool transport
    let requestTip = Transport.getTip transport

    match msg with
    | InProcMessage.Transactions msg ->
        match Transactions.deserialize Full msg.count msg.txs with
        | Some txs ->
            List.iter (Services.Blockchain.validateTransaction client ) txs

            connector,publisher,ownAddress
        | None ->
            //TODO: log non-deserializable transaction
            connector,publisher,ownAddress
    | InProcMessage.Connected {address=address;peerId=peerId} ->

        // We just connected to a remote peer, lets send him our address
        Option.iter (fun address ->
            Serialization.Address.serialize (address, now)
            |> Transport.sendAddress transport peerId) ownAddress

        // Request addresses and mempool
        requestAddresses peerId
        requestMemPool peerId
        requestTip peerId

        (Connector.connected connector address),publisher,ownAddress
    | InProcMessage.Accepted peerId ->

        // Request addresses and mempool
        requestAddresses peerId
        requestMemPool peerId
        requestTip peerId

        connector,publisher,ownAddress
    | InProcMessage.Disconnected address ->
        let connector = Connector.disconnected connector address
        (Connector.connect transport addressBook now connector),publisher,ownAddress
    | InProcMessage.GetAddresses peerId ->
        let addresses = AddressBook.getValidAddresses now addressBook

        addresses
        |> Serialization.Addresses.serialize
        |> Transport.sendAddresses transport peerId (List.length addresses |> uint32)
        connector, publisher,ownAddress

    | InProcMessage.Addresses msg ->
        match  Serialization.Addresses.deserialize msg.count msg.addresses with
        | None ->
            eventX "Received invalid addresses from peer"
            |> Log.warning

            connector, publisher,ownAddress // TODO: we should punish the sending node
        | Some addresses ->

            match List.map fst addresses |> List.forall Endpoint.isValid with
            | false ->
                eventX "Received invalid addresses from peer"
                |> Log.warning

                connector,publisher,ownAddress // TODO: we should punish the sending node
            | true ->
                // Filter own address
                let addresses =
                    match ownAddress with
                    | Some ownAddress -> List.filter (fun (a,_) -> a <> ownAddress) addresses
                    | None -> addresses

                // Publish new addresses
                List.filter (fun (address,_) -> not <| AddressBook.contains address addressBook) addresses
                |> List.map (Serialization.Address.serialize)
                |> List.shuffle
                |> List.truncate 10
                |> fun addresses ->
                    if not <| List.isEmpty addresses then
                        let count = List.length addresses |> uint32
                        let addresses = Array.concat addresses
                        Transport.publishAddresses transport count addresses

                AddressBook.addList now addresses addressBook
                let connector = Connector.connect transport addressBook now connector

                connector,publisher,ownAddress
    | InProcMessage.GetMemPool peerId ->
        Blockchain.requestMemPool client peerId

        connector, publisher,ownAddress
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

        connector, publisher,ownAddress
    | InProcMessage.GetTransactions {peerId=peerId;txHashes=bytes} ->

        if (Array.length bytes) % Hash.Length = 0 then
            let txHashes =
                Array.chunkBySize Hash.Length bytes
                |> Seq.ofArray
                |> Seq.map Hash.fromBytes
                |> Seq.choose id // We know all should pass as we already checked the size
                |> Seq.toList

            Blockchain.requestTransactions client peerId txHashes

        connector, publisher,ownAddress
    | InProcMessage.BlockRequest {peerId=peerId;blockHash=blockHash} ->
        match Hash.fromBytes blockHash with
        | Some blockHash ->
            Blockchain.requestBlock client peerId blockHash
        | None -> ()

        connector, publisher,ownAddress
    | InProcMessage.GetTip peerId ->
        Blockchain.requestTip client peerId
        connector, publisher,ownAddress
    | InProcMessage.Block {peerId=peerId;block=block} ->
        match Block.deserialize block with
        | Some block ->
            Blockchain.validateBlock client peerId block
            connector,publisher,ownAddress
        | None ->
            //TODO: log non-deserializable block
            connector,publisher,ownAddress
    | InProcMessage.Tip {peerId=peerId;blockHeader=blockHeader} ->
        match Header.deserialize blockHeader with
        | Some blockHeader ->
            Blockchain.handleTip client peerId blockHeader
            connector,publisher,ownAddress
        | None ->
            //TODO: log non-deserializable blockheader
            connector,publisher,ownAddress
    | InProcMessage.NewBlock {peerId=peerId;blockHeader=blockHeader} ->
        match Header.deserialize blockHeader with
        | Some blockHeader ->
            Blockchain.validateNewBlockHeader client peerId blockHeader
            connector,publisher,ownAddress
        | None ->
            //TODO: log non-deserializable blockheader
            connector,publisher,ownAddress
    | InProcMessage.HeadersRequest {peerId=peerId;from=from;endHash=endHash} ->
        let from =
            Array.chunkBySize Hash.Length from
            |> Seq.choose Hash.fromBytes
            |> List.ofSeq

        match List.isEmpty from, Hash.fromBytes endHash with
        | false, Some endHash -> Blockchain.requestHeaders client peerId from endHash
        | _ -> ()

        connector,publisher,ownAddress
    | InProcMessage.Headers {peerId=peerId;headers=headers} ->
        let headers =
            Array.chunkBySize Serialization.SerializedHeaderSize headers
            |> Seq.choose Header.deserialize
            |> List.ofSeq

        Blockchain.handleHeaders client peerId headers

        connector,publisher,ownAddress
    | InProcMessage.NewTransactions {peerId=peerId;txHashes=bytes} ->
        if (Array.length bytes) % Hash.Length = 0 then
            let txHashes =
                Array.chunkBySize Hash.Length bytes
                |> Seq.ofArray
                |> Seq.map Hash.fromBytes
                |> Seq.choose id // We know all should pass as we already checked the size
                |> Seq.toList

            Blockchain.handleNewTransactions client peerId txHashes

        connector,publisher,ownAddress
    | InProcMessage.UpdateAddressTimestamp address ->
        AddressBook.add now address now addressBook

        connector,publisher,ownAddress

let commandHandler transport command (state:State) =
    match command with
    | Command.SendMemPool (peerId, txHashes) ->
        let bytes =
            List.map Hash.bytes txHashes
            |> Array.concat
        Transport.sendMemPool transport peerId bytes
        state
    | Command.GetTransactions (peerId, txHashes) ->
        let bytes = List.map Hash.bytes txHashes |> Array.concat

        Transport.getTransactions transport peerId bytes
        state
    | Command.SendTransactions (peerId, txs) ->
        let bytes = Transactions.serialize Full txs
        Transport.sendTransactions transport peerId (List.length txs |> uint32) bytes
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

let handleIpAddressFound bind transport now ipAddress (connector,publisher,ownAddress) =
    let port = Endpoint.getPort bind
    let address = sprintf "%s:%d" ipAddress port
    let ownAddress = Some address

    Transport.publishAddressToAll transport ((address,now) |> Serialization.Address.serialize)

    connector,publisher,ownAddress

let requestHandler (requestId:RequestId) request (state:State) =
    match request with
    | GetConnectionCount ->
        let (connector,_,_) = state

        Connector.countConnected connector
        |> uint32
        |> requestId.reply

        state

let handlePublisherTick transport (connector,publisher,ownAddress) =
    let publisher = TransactionPublisher.tick transport publisher

    connector,publisher,ownAddress

let handleOneDayTick transport now (connector,publisher,ownAddress) =

    Option.iter (fun address ->
        Transport.publishAddressToAll transport ((address,now) |> Serialization.Address.serialize)) ownAddress

    connector,publisher,ownAddress

let main dataPath busName chainParams externalIp listen bind seeds wipe seed =
    let dataPath = Platform.combine dataPath "networkdb"

    if wipe then
        eventX "Wiping network database"
        |> Log.info
        if System.IO.Directory.Exists dataPath then
                System.IO.Directory.Delete (dataPath,true)

    Actor.create<Command, Request, Event, State> busName serviceName (fun poller sbObservable ebObservable ->
        let transport = Transport.create listen bind chainParams.networkId

        let addressBook = AddressBook.create dataPath

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
            |> Connector.connect transport addressBook (Timestamp.now ())

        let publisher = TransactionPublisher.empty

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
            |> Observable.map (transportHandler transport seeds client addressBook (Timestamp.now ()))

        let discoverIpObservable, discoverIpDisposable =
            if Option.isNone ownAddress && listen && not seed then
                let discoverIp = DiscoverIP.create ()
                let observable =
                    DiscoverIP.addToPoller poller discoverIp
                    |> Observable.map (handleIpAddressFound bind transport (Timestamp.now ()))

                observable, Disposables.toDisposable discoverIp
            else
                FSharp.Control.Reactive.Observable.empty, Disposables.empty

        let publisherObservable =
            let timer = Timer.create 200<milliseconds>

            Poller.addTimer poller timer
            |> Observable.map (fun _ -> handlePublisherTick transport)

        let publishAddressObservable =
            let oneDay = 1000<milliseconds> * 60 * 60 * 24
            let timer = Timer.create oneDay

            Poller.addTimer poller timer
            |> Observable.map (fun _ -> handleOneDayTick transport (Timestamp.now ()))

        let observable =
            Observable.merge sbObservable ebObservable
            |> Observable.merge transportObservable
            |> Observable.merge discoverIpObservable
            |> Observable.merge publisherObservable
            |> Observable.merge publishAddressObservable
            |> Observable.scan (fun state handler -> handler state ) (connector, publisher,ownAddress)

        Disposables.fromList [Disposables.toDisposable transport;discoverIpDisposable] ,observable
    )

