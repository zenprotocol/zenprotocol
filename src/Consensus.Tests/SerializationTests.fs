module Consensus.Tests.SerializationTests

open Consensus
open Hash
open Types
open Transaction
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
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
    |> Transaction.serialize mode
    |> Transaction.deserialize mode = Some (txInMode mode tx)

[<Property>]
let ``Different transactions don't produce same serialization result``(mode:TransactionSerializationMode) (tx1:Transaction) (tx2:Transaction) =
    (txInMode mode tx1 <> txInMode mode tx2) ==> lazy (Transaction.serialize mode tx1 <> Transaction.serialize mode tx2)

[<Property>]
let ``Transaction hash size should be 32``(tx:Transaction) =
    let (Hash bytes) = Transaction.hash tx
    Array.length bytes = 32

[<Property>]
let ``Different transactions don't produce same hashing result``(tx1:Transaction) (tx2:Transaction) =
    (txInMode WithoutWitness tx1 <> txInMode WithoutWitness tx2) ==> lazy (
        Transaction.hash tx1 <> Transaction.hash tx2
    )

[<Property>]
let ``Block serialization round trip produces same result`` (bk:Block) =
    bk
    |> Block.serialize
    |> Block.deserialize = Some bk

[<Property>]
let ``Different blocks don't produce same serialization result`` (bk1:Block) (bk2:Block) =
    (bk1 <> bk2) ==> lazy (Block.serialize bk1 <> Block.serialize bk2)

[<Property>]
let ``Block hash size should be 32``(tx:Transaction) =
    let (Hash bytes) = Transaction.hash tx
    Array.length bytes = 32

[<Property>]
let ``Different blocks don't produce same hashing result``(bk1:Block) (bk2:Block) =
    (bk1 <> bk2) ==> lazy (
        Block.hash bk1.header <> Block.hash bk2.header
    )

open Zen.Types.Data

[<Property>]
let ``Data serialization round trip produces same result``(data:data) =
    data
    |> Data.serialize
    |> Data.deserialize = Some data

[<Property>]
let ``Different data don't produce same serialization result``(data1:data) (data2:data) =
    (data1 <> data2) ==> lazy (Data.serialize data1 <> Data.serialize data2)

