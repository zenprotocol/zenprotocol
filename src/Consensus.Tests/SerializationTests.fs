module Consensus.Tests.SerializationTests

open Consensus
open Hash
open Types
open Transaction
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Serialization
open FsUnit
open TestsInfrastructure.Constraints
open Infrastructure

let txInMode mode tx =
    match mode with
    | Full -> tx
    | WithoutWitness -> {tx with witnesses=[]}

[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<ConsensusGenerator>() |> ignore

[<Property(EndSize=10000)>]
let ``Transaction serialization round trip produces same result`` (tx:Transaction) =
    try
        tx
        |> Transaction.serialize Full
        |> Transaction.deserialize Full = Some tx
    with
    | :? System.NullReferenceException ->
        printf "null exception caught in tx round trip"
        false

[<Property(EndSize=10000)>]
let ``Different transactions don't produce same serialization result``(mode:TransactionSerializationMode) (tx1:Transaction) (tx2:Transaction) =
    (txInMode mode tx1 <> txInMode mode tx2) ==> lazy (Transaction.serialize mode tx1 <> Transaction.serialize mode tx2)

// If serialization is hard to collide, and hashes are hard to collide, then
// hashes of serializations are hard to collide.
// Therefore, it's not necessary to test tx1 <> tx2 ==> hash tx1 <> hash tx2.
// The same applies to hashes of blocks.

[<Property(EndSize=10000)>]
let ``Block-header serialization round trip produces same result`` (h:BlockHeader) =
    h
    |> Header.serialize
    |> Header.deserialize = Some h

[<Property(EndSize=10000)>]
let ``Different block headers don't produce same serialization result`` (h1:BlockHeader) (h2:BlockHeader) =
    (h1 <> h2) ==> lazy (Header.serialize h1 <> Header.serialize h2)

[<Property(EndSize=10000)>]
let ``Block serialization round trip produces same result`` (bk:Block) =
    bk
    |> Block.serialize
    |> Block.deserialize = Some bk

[<Property(EndSize=10000)>]
let ``Different blocks don't produce same serialization result`` (bk1:Block) (bk2:Block) =
    (bk1 <> bk2) ==> lazy (Block.serialize bk1 <> Block.serialize bk2)

open Consensus.Tests
open Zen.Types.Data

[<Property(EndSize=10000)>]
let ``Data serialization round trip produces same result``(data:data) =
    data
    |> Data.serialize
    |> Data.deserialize = Some data
        
[<Property(EndSize=10000)>]
let ``Different data don't produce same serialization result``(data1:data) (data2:data) =
    (data1 <> data2) ==> lazy (Data.serialize data1 <> Data.serialize data2)

[<Property(EndSize=10000)>]
let ``serialize and deserialize varint yield the same number``(num:uint32)  =
    let stream = FsNetMQ.Stream.create 5

    let stream =
        Serialization.Serialization.VarInt.write Serialization.Serialization.serializers num stream
        |> FsNetMQ.Stream.reset

    let num' = Serialization.Serialization.VarInt.read stream |> fst |> Option.get

    num = num'

let serialize x =
    Serialization.Amount.write Serialization.counters x 0ul
    |> int32
    |> FsNetMQ.Stream.create
    |> Serialization.Amount.write Serialization.serializers x
    |> FsNetMQ.Stream.getBuffer
let deserialize bytes =
    FsNetMQ.Stream.Stream (bytes, 0)
    |> FsNetMQ.Stream.Reader.run Serialization.Amount.read

[<Property(MaxTest=1000)>]
let ``Amount serialization round trip produces same result``() =
    Arb.generate<DoNotSize<uint64>>
    |> Gen.map (fun (DoNotSize x) -> x)
    |> Arb.fromGen
    |> Prop.forAll <| fun amt ->
        amt |> serialize |> deserialize |> Option.get = amt

[<Test>]
let ``Amount values less than 1E4 round trip``() =
    seq {0UL .. 10000UL}
    |> Seq.forall (fun amt ->
        amt |> serialize |> deserialize |> Option.get = amt)
    |> should be True

[<Test>]
let ``Amount values close to powers of two round trip``() =
    let s = [0..63] |> List.map (fun x -> pown 2UL x)
    let l = s |> List.map (fun x -> x - 1UL)
    let u = s |> List.map (fun x -> x + 1UL)
    List.concat [s;l;u]
    |> Seq.forall (fun amt ->
        amt |> serialize |> deserialize |> Option.get = amt)
    |> should be True

[<Test>]
let ``Amounts with at most 3 significant figures round trip``() =
    let s = seq {
         for sf in 0UL .. 999UL do
             for e in 0 .. 16 do
                yield sf * pown 10UL e
                }
    let bigs = seq {
            for sf in 0UL .. 184UL do
                yield sf * pown 10UL 17
            }

    let s' = Seq.concat [s; bigs]
    s'
    |> Seq.forall (fun amt ->
        amt |> serialize |> deserialize |> Option.get = amt)
    |> should be True

[<Test>]
let ``Amounts with at most 3 significant figures have size 2 bytes``() =
    let s = seq {
         for sf in 0UL .. 999UL do
             for e in 0 .. 16 do
                yield sf * pown 10UL e
                }
    let bigs = seq {
            for sf in 0UL .. 184UL do
                yield sf * pown 10UL 17
            }

    let s' = Seq.concat [s; bigs]
    s'
    |> Seq.forall (fun amt ->
        amt |> serialize |> Array.length = 2)
    |> should be True

[<Test>]
let ``Amounts with between 4 and 8 significant figures round trip``() =
    let s = seq {
        for r in 3 .. 8 do
            for e in 0 .. (19 - r) do
                if r < 8 then yield (pown 10UL r + 1UL) * (pown 10UL e)
                if r > 3 then yield (pown 10UL r - 1UL) * (pown 10UL e)
    }
    s
    |> Seq.forall (fun amt ->
        amt |> serialize |> deserialize |> Option.get = amt)
    |> should be True

[<Test>]
let ``Amounts with between 4 and 8 significant figures have size 4 bytes``() =
    let s = seq {
        for r in 3 .. 8 do
            for e in 0 .. (19 - r) do
                if r < 8 then yield (pown 10UL r + 1UL) * (pown 10UL e)
                if r > 3 then yield (pown 10UL r - 1UL) * (pown 10UL e)
    }
    s
    |> Seq.forall (fun amt ->
        amt |> serialize |> Array.length = 4)
    |> should be True

[<Test>]
let ``Amounts with more than 8 significant figures have size 8 or 9 bytes``() =
    let s = seq {
        for r in 8 .. 19 do
            for e in 0 .. (19 - r) do
                yield (pown 10UL r + 1UL) * (pown 10UL e)
    }
    s
    |> Seq.forall (fun amt ->
        amt |> serialize |> Array.length = (if amt < pown 2UL 56 then 8 else 9))
    |> should be True
