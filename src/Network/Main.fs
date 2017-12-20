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
open Network
open Network.Message
open Network.Transport

type State = Connector.T * AddressBook.T

let maxConnections = 3

let eventHandler transport event (connector,addressBook) = 
    match event with 
    | Event.TransactionAddedToMemPool (txHash, tx) ->
        let bytes = Transaction.serialize Transaction.Full tx        
            
        Transport.publishTransaction transport bytes
        connector,addressBook
    | _ -> connector,addressBook

let transportHandler transport client ownAddress msg (connector,addressBook) = 
    match msg with 
    | InProcMessage.Transaction msg ->
        match Transaction.deserialize msg with
        | Some tx ->
            Services.Blockchain.validateTransaction client tx
            connector,addressBook
        | None ->
            //TODO: log non-deserializable transaction
            connector,addressBook
    | InProcMessage.Connected {address=address;peerId=peerId} ->
                           
        // We just connected to a remote peer, lets send him our address
        Option.iter (fun address -> 
            Transport.sendAddress transport peerId address) ownAddress
            
        (Connector.connected connector address),addressBook                    
                            
    | InProcMessage.Disconnected address ->
        let connector = Connector.disconnected connector address  
        (Connector.connect transport addressBook connector),addressBook
    | InProcMessage.Address address ->
        let handleAddress () =                                 
            if not (AddressBook.contains addressBook address) then
                                
                // TODO: don't publish to the sender?
                Transport.publishAddress transport address
                
                let addressBook = AddressBook.add addressBook address
                
                // We might need more peers so lets try to connect to the new peer
                (Connector.connect transport addressBook connector), addressBook
            else connector,addressBook
                 
        match ownAddress with
        | None -> handleAddress ()
        | Some ownAddress when ownAddress <> address -> handleAddress ()
        | _ -> 
            // Own address, do nothing
            connector, addressBook   
                                          
    | _ -> 
        // TODO: log unknown message
        connector, addressBook

let commandHandler command (state:State) = state

let requestHandler request reply (state:State) = state

let main busName externalIp listen bind seeds =
    Actor.create<Command, Request, Event, State> busName serviceName (fun poller sbObservable ebObservable ->           
        let transport = Transport.create listen bind
        
        let addressBook = AddressBook.create seeds
        
        let ownAddress = 
            if not (System.String.IsNullOrEmpty externalIp) && listen then 
                let port = Endpoint.getPort bind
                
                Log.info "Public IP: %s" externalIp
                Some (sprintf "%s:%d" externalIp port)                
            else None
        
        let connector = 
            Connector.create maxConnections
            |> Connector.connect transport addressBook
        
        let client = ServiceBus.Client.create busName
        
        let sbObservable = 
            sbObservable
            |> Observable.map (fun message ->
                match message with 
                | ServiceBus.Agent.Command c -> commandHandler c 
                | ServiceBus.Agent.Request (r, reply) -> requestHandler r reply)                
        
        let ebObservable = 
            ebObservable
            |> Observable.map (eventHandler transport)
            
        let transportObservable = 
            Transport.addToPoller poller transport            
            |> Observable.map (fun _ -> Transport.recv transport)            
            |> Observable.map (transportHandler transport client ownAddress)
           
        let observable =             
            Observable.merge sbObservable ebObservable
            |> Observable.merge transportObservable            
            |> Observable.scan (fun state handler -> handler state) (connector,addressBook)
    
        Disposables.toDisposable transport,observable
    )
                    
