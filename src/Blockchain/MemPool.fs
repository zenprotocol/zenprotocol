module Blockchain.MemPool

open Consensus
open Consensus.Types

type T = Map<Hash.Hash, Transaction>

let create () = Map.empty

let containsTransaction = Map.containsKey 

let add = Map.add