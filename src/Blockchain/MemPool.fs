module Blockchain.MemPool

open Consensus
open Consensus.Types

type T = Map<Hash.Hash, Transaction>

let create () = Map.empty

let containsTransaction = Map.containsKey 

let add = Map.add

let getTxHashes (mempool:T) = 
    Map.toSeq mempool
    |> Seq.map fst
    |> Seq.toList
    
let getTransaction = Map.tryFind