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

[<Property>]
let ``Transaction serialization round trip produces same result`` (tx:Transaction) =
    tx
    |> Transaction.serialize Full
    |> Transaction.deserialize Full = Some tx

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
let ``Block-header serialization round trip produces same result`` (h:BlockHeader) =
    h
    |> Header.serialize
    |> Header.deserialize = Some h

[<Property>]
let ``Different block headers don't produce same serialization result`` (h1:BlockHeader) (h2:BlockHeader) =
    (h1 <> h2) ==> lazy (Header.serialize h1 <> Header.serialize h2)

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

open Consensus.Tests
open Zen.Types.Data

[<Property>]
let ``Data serialization round trip produces same result``(data:data) =
    data
    |> Data.serialize
    |> Data.deserialize = Some data

[<Property>]
let ``Different data don't produce same serialization result``(data1:data) (data2:data) =
    (data1 <> data2) ==> lazy (Data.serialize data1 <> Data.serialize data2)

[<Property(StartSize=1,EndSize=1000000,MaxTest=1000)>]
let ``serialize and deserialize varint yield the same number``(num:uint32)  =
    let stream = FsNetMQ.Stream.create 5

    let stream =
        Serialization.Serialization.VarInt.write Serialization.Serialization.serializers num stream
        |> FsNetMQ.Stream.reset

    let num' = Serialization.Serialization.VarInt.read stream |> fst |> Option.get

    num = num'

module ZData = Zen.Types.Data

[<Test>]
let ``serialize and deserialize transaction with contract witness and data``() =

    let data =
        let returnAddress = PK Helper.rootPKHash

        Zen.Dictionary.add "returnAddress"B (ZData.Lock (ZFStar.fsToFstLock returnAddress)) Zen.Dictionary.empty
        |> Zen.Cost.Realized.__force
        |> ZData.DataDict
        |> ZData.Dict
        |> Some

    let tx : Transaction =
        {
            version = 0u;
            inputs =
                [
                    Outpoint {txHash = Hash.fromString "0342058085439b0d8155d6256c99128a7a0b8f6433fa3fd2db4e9f5f58eddcac" |> Result.get; index = 0u;};
                    Mint {asset = Asset.fromString "0000000007e4a6b0ba5d28bbef51700431ec50ba6839a0e9a20ca1b292961a0e71d4e54d" |> Option.get; amount = 1UL;}
                ];
            outputs =
                [
                    {lock = PK (Hash.fromString "0c1a5a801195c26fbb6a40160f5af67458967ef3afde9a6b614182d965d169ca" |> Result.get); spend = {asset = Asset.Zen; amount = 5000001630UL;};};
                    {lock = Contract (ContractId.fromString "0000000007e4a6b0ba5d28bbef51700431ec50ba6839a0e9a20ca1b292961a0e71d4e54d" |> Option.get); spend = {asset = Asset.Zen; amount = 1UL;};};
                    {lock = PK (Hash.fromString "0c1a5a801195c26fbb6a40160f5af67458967ef3afde9a6b614182d965d169ca" |> Result.get); spend = {asset = Asset.fromString "0000000007e4a6b0ba5d28bbef51700431ec50ba6839a0e9a20ca1b292961a0e71d4e54d" |> Option.get; amount = 1UL;};}
                ];
            witnesses =
                [
                    PKWitness (
                        Crypto.PublicKey
                           [|203uy; 239uy; 119uy; 124uy; 99uy; 101uy; 6uy; 197uy; 40uy; 51uy;
                             42uy; 236uy; 201uy; 77uy; 235uy; 25uy; 110uy; 204uy; 135uy; 112uy;
                             39uy; 121uy; 15uy; 59uy; 146uy; 231uy; 107uy; 18uy; 255uy; 161uy;
                             103uy; 79uy; 219uy; 197uy; 202uy; 128uy; 102uy; 159uy; 230uy; 135uy;
                             91uy; 65uy; 57uy; 218uy; 50uy; 7uy; 19uy; 58uy; 222uy; 149uy; 44uy;
                             66uy; 73uy; 49uy; 234uy; 159uy; 153uy; 245uy; 34uy; 154uy; 189uy;
                             23uy; 139uy; 80uy|],
                        Crypto.Signature
                           [|124uy; 234uy; 247uy; 118uy; 251uy; 215uy; 119uy; 166uy; 41uy; 229uy;
                             99uy; 107uy; 254uy; 97uy; 232uy; 127uy; 116uy; 131uy; 124uy; 93uy;
                             53uy; 157uy; 28uy; 11uy; 239uy; 137uy; 205uy; 121uy; 21uy; 165uy;
                             128uy; 61uy; 104uy; 77uy; 44uy; 197uy; 80uy; 194uy; 97uy; 130uy;
                             147uy; 204uy; 133uy; 46uy; 114uy; 222uy; 206uy; 87uy; 37uy; 228uy;
                             112uy; 65uy; 184uy; 78uy; 36uy; 120uy; 113uy; 139uy; 91uy; 175uy;
                             46uy; 17uy; 206uy; 117uy|]);
                    ContractWitness
                        {contractId = ContractId.fromString "0000000007e4a6b0ba5d28bbef51700431ec50ba6839a0e9a20ca1b292961a0e71d4e54d" |> Option.get;
                         command = "buy";
                         data = data
                         beginInputs = 1u;
                         beginOutputs = 1u;
                         inputsLength = 1u;
                         outputsLength = 2u;
                         signature = None;
                         cost = 570u;}
                ];
            contract = None;
        }

    tx |> Transaction.serialize Full  |> Transaction.deserialize Full |> should equal (Some tx)
