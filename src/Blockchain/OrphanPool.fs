module Blockchain.OrphanPool

open Consensus
open Consensus.Types
open Infrastructure

type T = Map<Hash.Hash, Transaction>

let create () = Map.empty

let add = Map.add

let containsTransaction = Map.containsKey

let foldWriter f state orphanPool = Map.foldBack (fun hash tx state  ->
    Writer.bind state (fun state -> f state hash tx)) orphanPool (Writer.ret state) 

let remove = Map.remove