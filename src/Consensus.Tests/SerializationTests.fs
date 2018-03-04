module Consensus.Tests.TransactionSerializationTests

open Consensus
open Consensus.Types
open Consensus.Hash
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit
open Serialization

let txInMode mode tx =
    match mode with
    | Full -> tx
    | WithoutWitness -> {tx with witnesses=[]}

[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<ConsensusGenerator>() |> ignore

[<Property>]
let ``Transaction serialization round trip produces same result``(mode:TransactionSerializationMode) (tx:Transaction) =
    tx
    |> serializeTransaction mode
    |> deserializeTransaction = Some (txInMode mode tx)

[<Property>]
let ``Different transactions don't produce same serialization result``(mode:TransactionSerializationMode) (tx1:Transaction) (tx2:Transaction) =
    (txInMode mode tx1 <> txInMode mode tx2) ==> lazy (serializeTransaction mode tx1 <> serializeTransaction mode tx2)

[<Property>]
let ``Hash size should be 32``(tx:Transaction) =
    let (Hash bytes) = Transaction.hash tx
    Array.length bytes = 32

[<Property>]
let ``Different transactions don't produce same hashing result``(tx1:Transaction) (tx2:Transaction) =
    (txInMode WithoutWitness tx1 <> txInMode WithoutWitness tx2) ==> lazy (
        Transaction.hash tx1 <> Transaction.hash tx2
    )
