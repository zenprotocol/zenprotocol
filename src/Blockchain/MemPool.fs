module Blockchain.MemPool

open Consensus
open Consensus.Types

type T = Map<Hash.Hash, uint32*TransactionExtended>

let empty = Map.empty

let isEmpty = Map.isEmpty

let toList (mempool:T) =
    Map.toList mempool
    |> List.sortBy (snd >> fst)
    |> List.map (fun (_,(_,ex)) -> ex)

let containsTransaction = Map.containsKey

let add ex (mempool:T) =
    let max =
        if isEmpty mempool then 0ul else
            mempool |> Map.toSeq |> Seq.map (snd >> fst) |> Seq.max

    let mempool = Map.add ex.txHash (max + 1ul, ex) mempool

    mempool

let getTxHashes : T -> Hash.Hash list =
    Map.toList >> List.sortBy (snd >> fst) >> List.map fst

let getTransactions : T -> TransactionExtended list =
    Map.toList >> List.map snd >> List.sortBy fst >> List.map snd

let getTransaction key map = Map.tryFind key map |> Option.map snd

let handleBlock block mempool =

    // remove all transactions in the block from the mempool    
    List.fold (fun mempool ex -> Map.remove ex.txHash mempool) mempool block.transactions

let undoBlock block mempool =

    let addTransactions mempool max txs =
        List.fold (fun (mempool, max) ex -> Map.add ex.txHash (max + 1ul, ex) mempool, max + 1ul) (mempool,max) txs

    // insert all transactions in the block to the mempool
    let mempool',max = block.transactions |> List.tail |> addTransactions empty 0ul

    let mempool,_ =
        toList mempool
        |> addTransactions mempool' max

    mempool
