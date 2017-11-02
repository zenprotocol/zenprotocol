module Consensus.Tests.TransactionTests

open Consensus
open Consensus.Types
open FsCheck.Xunit
open FsCheck

let txInMode mode tx =
    match mode with
    | Transaction.Full -> tx
    | Transaction.WithoutWitness -> {tx with witnesses=[]}

[<Property>]
let ``Transaction serialization round trip produces same result``(mode:Transaction.SerializationMode) (tx:Transaction) =
    tx
    |> Transaction.serialize mode
    |> Transaction.deserialize = txInMode mode tx

[<Property>]
let ``Different transactions don't produce same serialization result``(mode:Transaction.SerializationMode) (tx1:Transaction) (tx2:Transaction) =
    (txInMode mode tx1 <> txInMode mode tx2) ==> (Transaction.serialize mode tx1 <> Transaction.serialize mode tx2)