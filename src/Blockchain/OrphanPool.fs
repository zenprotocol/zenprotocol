module Blockchain.OrphanPool

open Consensus
open Consensus.Types
open Infrastructure

type T = Map<Hash.Hash, TransactionExtended>

let create () = Map.empty

let add ex = Map.add ex.txHash ex

let containsTransaction = Map.containsKey

let foldWriter f state orphanPool = Map.foldBack (fun _ ex state  ->
    Writer.bind state (fun state -> f state ex)) orphanPool (Writer.ret state) 

let remove = Map.remove