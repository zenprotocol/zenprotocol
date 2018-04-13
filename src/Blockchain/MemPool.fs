module Blockchain.MemPool

open Blockchain
open Consensus
open Consensus.Types

type T = Map<Hash.Hash, uint32*Transaction>

let empty = Map.empty

let isEmpty = Map.isEmpty

let toList (mempool:T) =
    Map.toList mempool
    |> List.sortBy (snd >> fst)
    |> List.map (fun (txHash,(_,tx)) -> txHash,tx)

let containsTransaction = Map.containsKey

let add txHash tx (mempool:T) =
    let max =
        if isEmpty mempool then 0ul else
            mempool |> Map.toSeq |> Seq.map (snd >> fst) |> Seq.max

    let mempool = Map.add txHash (max + 1ul, tx) mempool

    mempool

let getTxHashes : T -> Hash.Hash list =
    Map.toList >> List.sortBy (snd >> fst) >> List.map fst

let getTransactions : T -> Transaction list =
    Map.toList >> List.map snd >> List.sortBy fst >> List.map snd

let getTransaction key map = Map.tryFind key map |> Option.map snd

let handleBlock block mempool =

    // remove all transactions in the block from the mempool
    List.map Transaction.hash block.transactions
    |> List.fold (fun mempool txHash -> Map.remove txHash mempool) mempool

let undoBlock block mempool =

    let addTransactions mempool max txs =
        List.fold (fun (mempool, max) (txHash,tx) -> Map.add txHash (max + 1ul, tx) mempool, max + 1ul) (mempool,max) txs

    // insert all transactions in the block to the mempool
    let mempool',max = List.map (fun tx -> Transaction.hash tx, tx) block.transactions |> addTransactions empty 0ul

    let mempool,_ =
        toList mempool
        |> addTransactions mempool' max

    mempool
