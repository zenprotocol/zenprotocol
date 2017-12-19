module Network.Tests.TransportHelper

open Network
open Network.Transport
open FsNetMQ

let address = "127.0.0.1:5556"
let addressBook = AddressBook.create (seq {yield address})

let startPeer listener = 
    let transport = Transport.create listener address
    
    if not listener then
        Transport.connect transport address
        
    transport
    
let stopPeer peer = 
    (peer :> System.IDisposable).Dispose()   
    
let rec waitForConnectedMessage peer timeout = 
    match Transport.tryRecv peer timeout with 
    | Some (InProcMessage.Connected _) ->
        ()
    | Some (InProcMessage.Disconnected _) ->
        failwithf "connect failed, disconnect"
    | Some x ->                 
        failwithf "connect failed, unexpected %A" x
    | None ->                
        failwith "connect failed"
    
let rec waitForDisconnectedMessage peer timeout = 
    match Transport.tryRecv peer timeout with 
    | Some (InProcMessage.Disconnected _) ->
        ()
    | Some _                 
    | None ->                
        failwith "no disconnect message"        
        
let rec waitForAcceptedMessage peer timeout = 
    match Transport.tryRecv peer timeout with 
    | Some (InProcMessage.Accepted) ->
        ()
    | Some _                 
    | None ->                
        failwith "accepted failed"        
                
        