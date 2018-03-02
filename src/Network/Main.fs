module Network.Main

open System.Net
open FSharp.Control
open Network
open FsNetMQ
open Infrastructure
open Messaging.Services.Network
open Messaging.Events
open Consensus
open Messaging
open Messaging.Services
open Network
open Network.Message
open Network.Transport

type State = Connector.T * AddressBook.T * string option

let maxConnections = 3

let eventHandler transport event (connector,addressBook, ownAddress) = 
    match event with 
    | Event.TransactionAddedToMemPool (txHash, tx) ->
        let bytes = TransactionSerialization.serialize TransactionSerialization.Full tx        
            
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
        match TransactionSerialization.deserialize msg with
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
            Log.warning "Received invalid address from peer %s" address 
            connector, addressBook,ownAddress // TODO: we should punish the sending node
        | true ->            
            let handleAddress () =
                Log.info "Received new address %s" address
                                             
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
            Log.warning "Received invalid addresses from peer"
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
    | InProcMessage.Block block ->
        match Block.deserialize block with
        | Some block ->
            Blockchain.validateBlock client block
            connector,addressBook,ownAddress
        | None ->
            //TODO: log non-deserializable block
            connector,addressBook,ownAddress
    | InProcMessage.Tip blockHeader ->
        match BlockHeader.deserialize blockHeader with
        | Some blockHeader ->
            Blockchain.handleTip client blockHeader
            connector,addressBook,ownAddress
        | None ->
            //TODO: log non-deserializable blockheader
            connector,addressBook,ownAddress
    | InProcMessage.NewBlock {peerId=peerId;blockHeader=blockHeader} ->
        match BlockHeader.deserialize blockHeader with
        | Some blockHeader ->
            Blockchain.validateNewBlockHeader client peerId blockHeader
            connector,addressBook,ownAddress
        | None ->
            //TODO: log non-deserializable blockheader
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
        let bytes = TransactionSerialization.serialize TransactionSerialization.Full tx
        Transport.sendTransaction transport peerId bytes         
        state
    | Command.SendBlock (peerId, block) ->
        let bytes = Block.serialize block
        Transport.sendBlock transport peerId bytes         
        state
    | Command.SendTip (peerId,blockHeader) ->
        let bytes = BlockHeader.serialize blockHeader
        Transport.sendTip transport peerId bytes
        state
    | Command.GetBlock blockHash ->
        Transport.getBlock transport (Hash.bytes blockHash)
        state
    | Command.PublishBlock blockHeader ->
        let bytes = BlockHeader.serialize blockHeader
        Transport.publisNewBlock transport bytes

        state
    | Command.GetNewBlock (peerId,blockHash) ->
        Transport.getNewBlock transport peerId (Hash.bytes blockHash)
        state
        
let handleIpAddressFound bind transport ipAddress (connector,addressBook,ownAddress) = 
    let port = Endpoint.getPort bind
    let address = sprintf "%s:%d" ipAddress port
    let ownAddress = Some address
    
    Transport.publishAddressToAll transport address 

    connector,addressBook,ownAddress     
                      
let requestHandler request reply (state:State) = state

let main busName externalIp listen bind seeds =
    Actor.create<Command, Request, Event, State> busName serviceName (fun poller sbObservable ebObservable ->           
        let transport = Transport.create listen bind
        
        let addressBook = AddressBook.empty
        
        let ownAddress = 
            if not (System.String.IsNullOrEmpty externalIp) && listen then 
                let port = Endpoint.getPort bind
                
                Log.info "Public IP: %s" externalIp
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
                | ServiceBus.Agent.Request (r, reply) -> requestHandler r reply)                
        
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
                    
