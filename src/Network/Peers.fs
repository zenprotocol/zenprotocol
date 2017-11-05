module Peers

open FsNetMQ
open Network

type T = Map<RoutingId.T,Peer.Peer>

let empty = Map.empty

let private mapToPeers (_,peer) = peer

let connectingAddresses:T->seq<_> =    
    Map.toSeq >> Seq.map (fun (_,peer) -> peer) >> Seq.filter Peer.isConntecting >> Seq.choose Peer.getAddress  
    
let activeAndConnecting:T->seq<_> =     
    Map.filter (fun _ -> Peer.isDead >> not) >> Map.toSeq >> Seq.map mapToPeers
    
let activePeers:T->seq<_> =     
    Map.filter (fun _ -> Peer.isActive) >> Map.toSeq >> Seq.map mapToPeers
    
let connect peers socket addresses =
    Seq.map (Peer.connect socket) addresses
    |> Seq.filter (Peer.isDead >> not)
    |> Seq.fold (fun peers peer -> Map.add (Peer.routingId peer) peer peers) peers 
    
let isActive peers = 
    Map.exists (fun _ -> Peer.isActive) peers
    
let tryGet peers key = Map.tryFind key peers

let update peers key peer = Map.add key peer peers

let remove peers key = Map.remove key peers

let run peers f = 
    Map.map (fun _ peer -> f peer) peers
    |> Map.filter (fun _ -> Peer.isDead >> not)