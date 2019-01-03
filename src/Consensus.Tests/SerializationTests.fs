module Consensus.Tests.SerializationTests

open Consensus
open Hash
open Types
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Serialization
open FsUnit
open Infrastructure
open Consensus.Tests

let txInMode mode tx =
    match mode with
    | Full -> tx
    | WithoutWitness -> {tx with witnesses=[]}

[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<ConsensusGenerator>() |> ignore

[<Property>]
let ``Raw Transaction serialization round trip produces same result`` (tx:RawTransaction) =
    tx
    |> RawTransaction.serialize
    |> RawTransaction.deserialize = Some tx

[<Property(EndSize=2000)>]
let ``Transaction serialization round trip produces same result`` (tx:Transaction) =
    tx
    |> Transaction.serialize Full
    |> Transaction.deserialize Full = Some tx

[<Property(EndSize=2000)>]
let ``Different transactions don't produce same serialization result``(mode:TransactionSerializationMode) (tx1:Transaction) (tx2:Transaction) =
    (txInMode mode tx1 <> txInMode mode tx2) ==> lazy (Transaction.serialize mode tx1 <> Transaction.serialize mode tx2)

// If serialization is hard to collide, and hashes are hard to collide, then
// hashes of serializations are hard to collide.
// Therefore, it's not necessary to test tx1 <> tx2 ==> hash tx1 <> hash tx2.
// The same applies to hashes of blocks.

[<Property(EndSize=2000)>]
let ``Block-header serialization round trip produces same result`` (h:BlockHeader) =
    h
    |> Header.serialize
    |> Header.deserialize = Some h

[<Property(EndSize=2000)>]
let ``Different block headers don't produce same serialization result`` (h1:BlockHeader) (h2:BlockHeader) =
    (h1 <> h2) ==> lazy (Header.serialize h1 <> Header.serialize h2)

[<Property(EndSize=2000)>]
let ``Block serialization round trip produces same result`` (bk:Block) =
    bk
    |> Block.serialize
    |> Block.deserialize = Some bk

[<Property(EndSize=2000)>]
let ``Different blocks don't produce same serialization result`` (bk1:Block) (bk2:Block) =
    (bk1 <> bk2) ==> lazy (Block.serialize bk1 <> Block.serialize bk2)

open Consensus.Tests
open Zen.Types.Data

[<Property(EndSize=2000)>]
let ``Data serialization round trip produces same result``(data:data) =
    data
    |> Data.serialize
    |> Data.deserialize = Some data

[<Property(EndSize=2000)>]
let ``Different data don't produce same serialization result``(data1:data) (data2:data) =
    (data1 <> data2) ==> lazy (Data.serialize data1 <> Data.serialize data2)

let serializeAmount x = serialize Serialization.Amount.size Serialization.Amount.write x
let serializeAsset x = serialize Serialization.Asset.size Serialization.Asset.write x
let serializeVarInt x = serialize Serialization.VarInt.size Serialization.VarInt.write x
let deserializeAmount x = deserialize Serialization.Amount.read x
let deserializeAsset x = deserialize Serialization.Asset.read x
let deserializeVarInt x = deserialize Serialization.VarInt.read x

[<Property(EndSize=2000)>]
let ``serialize and deserialize varint yield the same number``(num:uint32)  =
    let bytes = serializeVarInt num

    let num' = deserializeVarInt bytes |> Option.get

    num = num'

[<Property(MaxTest=1000)>]
let ``Amount serialization round trip produces same result``() =
    Arb.generate<DoNotSize<uint64>>
    |> Gen.map (fun (DoNotSize x) -> x)
    |> Arb.fromGen
    |> Prop.forAll <| fun amt ->
        amt |> serializeAmount |> deserializeAmount |> Option.get = amt

[<Test>]
let ``Amount values less than 1E4 round trip``() =
    seq {0UL .. 10000UL}
    |> Seq.forall (fun amt ->
        amt |> serializeAmount |> deserializeAmount |> Option.get = amt)
    |> should be True

[<Test>]
let ``Amount values close to powers of two round trip``() =
    let s = [0..63] |> List.map (fun x -> pown 2UL x)
    let l = s |> List.map (fun x -> x - 1UL)
    let u = s |> List.map (fun x -> x + 1UL)
    List.concat [s;l;u]
    |> Seq.forall (fun amt ->
        amt |> serializeAmount |> deserializeAmount |> Option.get = amt)
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
        amt |> serializeAmount |> deserializeAmount |> Option.get = amt)
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
        amt |> serializeAmount |> Array.length = 2)
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
        amt |> serializeAmount |> deserializeAmount |> Option.get = amt)
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
        amt |> serializeAmount |> Array.length = 4)
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
        amt |> serializeAmount |> Array.length = (if amt < pown 2UL 56 then 8 else 9))
    |> should be True

[<Test>]
let ``Asset round trips when Zen native token``() =
    Asset.Zen |> serializeAsset |> deserializeAsset |> Option.get
    |> should equal Asset.Zen

[<Property>]
let ``Asset round trips when default contract issued``(cHash:Hash) =
    let defaultAsset = Asset.defaultOf (ContractId (0u, cHash))
    defaultAsset
    |> serializeAsset |> deserializeAsset |> Option.get = defaultAsset

[<Property>]
let ``Asset round trips for any contract subtype``(cHash:Hash,subtype:Hash) =
    let asset = Asset (ContractId(0u, cHash), subtype)
    asset
    |> serializeAsset |> deserializeAsset |> Option.get = asset

[<Property>]
let ``Asset round trips for small versions, any subtype``((SmallVersion version), cHash:Hash, subtype:Hash) =
    let asset = Asset (ContractId(version, cHash), subtype)
    asset
    |> serializeAsset |> deserializeAsset |> Option.get = asset

[<Property>]
let ``Asset round trips for higher versions, any subtype``((DoNotSize version):DoNotSize<uint32>,cHash:Hash,subtype:Hash) =
    let asset = Asset (ContractId(version, cHash), subtype)
    asset
    |> serializeAsset |> deserializeAsset |> Option.get = asset

[<Property>]
let ``Asset round trips for compressible subtypes``(version:uint32,cHash:Hash,(CompressibleSubtype subtype)) =
    let asset = Asset (ContractId(version, cHash), subtype)
    asset
    |> serializeAsset |> deserializeAsset |> Option.get = asset

[<Property>]
let ``Asset is compressed for compressible subtypes``((SmallVersion version), cHash:Hash, (CompressibleSubtype subtype)) =
    let asset = Asset (ContractId(version, cHash), subtype)
    Array.length (serializeAsset asset) <= 64

[<Property>]
let ``Asset has correct size for non-compressible subtypes``((SmallVersion version), cHash:Hash, (Hash.Hash sbs as subtype)) =
    let asset = Asset (ContractId(version, cHash), subtype)
    (cHash <> Hash.zero && sbs.[Length-1] <> 0uy)
    ==> (Array.length (serializeAsset asset) = 65)

[<Test>]
let ``Asset has size 1 for Zen native token``() =
    Asset.Zen |> serializeAsset |> Array.length
    |> should equal 1

[<Property>]
let ``Asset has size 33 for default contract issued and small version``((SmallVersion version), cHash:Hash) =
    let asset = Asset.defaultOf (ContractId (version, cHash))
    (cHash <> Hash.zero)
    ==> (Array.length (serializeAsset asset) = 33)

[<Property>]
let ``Asset has size 34 for default contract issued and one byte version``((OneByteVersion version), cHash:Hash) =
    let asset = Asset.defaultOf (ContractId (version, cHash))
    (cHash <> Hash.zero)
    ==> (Array.length (serializeAsset asset) = 34)

[<Property>]
let ``Asset has size 35 for default contract issued and two byte version``((TwoByteVersion version), cHash:Hash) =
    let asset = Asset.defaultOf (ContractId (version, cHash))
    (cHash <> Hash.zero)
    ==> (Array.length (serializeAsset asset) = 35)

[<Property>]
let ``Asset has size 36 for default contract issued and three byte version``((ThreeByteVersion version), cHash:Hash) =
    let asset = Asset.defaultOf (ContractId (version, cHash))
    (cHash <> Hash.zero)
    ==> (Array.length (serializeAsset asset) = 36)

[<Property>]
let ``Asset has size 37 for default contract issued and four byte version``((FourByteVersion version), cHash:Hash) =
    let asset = Asset.defaultOf (ContractId (version, cHash))
    (cHash <> Hash.zero)
    ==> (Array.length (serializeAsset asset) = 37)

[<Property>]
let ``Asset round-trips when zero hash and subtype but non-zero version``(DoNotSize version : DoNotSize<uint32>) =
    let asset = Asset.defaultOf (ContractId (version, Hash.zero))
    version <> 0u
    ==> (asset |> serializeAsset |> deserializeAsset |> Option.get = asset)

[<Property>]
let ``Asset has size 65 with zero type, incompressible subtype, small version``((SmallVersion version), (Hash.Hash sbs as subtype)) =
    let asset = Asset (ContractId(version, Hash.zero), subtype)
    (sbs.[Length-1] <> 0uy || (sbs.[Length-1] = 0uy && sbs.[Length-2] <> 0uy))
    ==> (Array.length (serializeAsset asset) = 65)

[<Property>]
let ``Raw transactions equal extended transactions``(NonEmptyTransactions txs) =
    let raw =
        List.map (fun tx -> tx.raw) txs
        |> TransactionsRaw.serialize

    let result = TransactionsExtended.deserialize (List.length txs |> uint32) raw = Some txs

    if not result then
        printfn "%A" <| TransactionsExtended.deserialize (List.length txs |> uint32) raw

    result

let checkSize sizeFn writeFn value =
    let size = sizeFn value
    let stream = Stream(Array.zeroCreate size)
    writeFn stream value

    stream.Offset = size

let checkRoundtrip sizeFn writeFn readFn value =
    let size = sizeFn value
    let stream = Stream(Array.zeroCreate size)
    writeFn stream value

    readFn (Stream(stream.Buffer)) = value

open Serialization

[<Property>]
let ``CGP wrote equal to Data.size``(data:CGP.T) =
    checkSize CGP.size CGP.write data

[<Property>]
let ``Tally wrote equal to Data.size``(data:Tally.T) =
    checkSize CGP.Tally.size CGP.Tally.write data

[<Property>]
let ``Lock wrote equal to Data.size``(data:VoteData) =
    checkSize VoteData.size VoteData.write data

[<Property>]
let ``CGP serialization round trip produces same result``(data:CGP.T) =
    checkRoundtrip CGP.size CGP.write CGP.read data

[<Property>]
let ``Tally serialization round trip produces same result``(data:Tally.T) =
    checkRoundtrip CGP.Tally.size CGP.Tally.write CGP.Tally.read data

[<Property>]
let ``Lock serialization round trip produces same result``(data:VoteData) =
    checkRoundtrip VoteData.size VoteData.write VoteData.read data

[<Property>]
let ``Data amount wrote equal to Data.size``(data:data) =
    checkSize Serialization.Data.size Serialization.Data.write data

[<Property>]
let ``Output amount wrote equal to Output.size``(output:Output) =
    checkSize Serialization.Output.size Serialization.Output.write output

[<Property>]
let ``Input amount wrote equal to Input.size``(input:Input) =
    checkSize Serialization.Input.size Serialization.Input.write input

[<Property>]
let ``Wintess amount wrote equal to Wintess.size``(witness:Witness) =
    checkSize Serialization.Witness.size Serialization.Witness.write witness

[<Property>]
let ``Transaction amount wrote equal to Transaction.size``(tx:Transaction) =
    checkSize (Serialization.Transaction.size Full) (Serialization.Transaction.write Full) tx

[<Property>]
let ``Block amount wrote equal to Block.size``(tx:Block) =
    checkSize Serialization.Block.size Serialization.Block.write tx
