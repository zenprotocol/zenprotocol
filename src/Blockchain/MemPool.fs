module Blockchain.MemPool

open Consensus
open Consensus.Types

type T = Map<Hash.Hash, Transaction>

let empty = Map.empty

let isEmpty = Map.isEmpty

let containsTransaction = Map.containsKey 

let add = Map.add

let getTxHashes (mempool:T) = 
    Map.toSeq mempool
    |> Seq.map fst
    |> Seq.toList
    
let getTransaction = Map.tryFind

let handleBlock block (mempool:T) = 

    // removing all transaction in the block from the mempool    
    List.map Transaction.hash block.transactions
    |> List.fold (fun mempool txHash -> Map.remove txHash mempool) mempool

let undoBlock block (mempool:T) =

    // readd all transaction in the block to the mempool 
    List.map (fun tx -> Transaction.hash tx, tx) block.transactions
    |> List.fold (fun mempool (txHash,tx) -> Map.add txHash tx mempool) mempool
