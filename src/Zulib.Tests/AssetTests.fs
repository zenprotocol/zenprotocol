module Zulib.Tests.AssetTests

open Helper
open NUnit.Framework
open FsUnit
open Infrastructure
open FsCheck
open FsCheck.NUnit
open FStar.Pervasives.Native

module Arbitrary =
    //TODO: redundant - remove wrapping types
    type ContractId = ContractId of Zen.Types.Extracted.contractId
    type Hash = Hash of byte[]
    type SubtypeString = SubtypeString of Prims.string
    type Asset = Asset of Zen.Types.Extracted.asset

    type Generators =
        static member string() =
            Arb.fromGen <| gen {
                let! value =
                    Arb.generate<Prims.string>
                    |> Gen.filter ((<>) null)
                    |> Gen.filter (fun s -> Seq.length s < 29)
                return SubtypeString value
            }
        static member hash() =
            Arb.fromGen <| gen {
                let! bytes = Gen.arrayOfLength Consensus.Hash.Length Arb.generate<byte>
                return Hash bytes
            }
        static member contractId() =
            Arb.fromGen <| gen {
                let! version = Arb.generate<FStar.UInt32.t>
                let! (Hash hash) = Arb.generate<Hash>
                return ContractId (version, hash : Zen.Types.Extracted.contractId)
            }
        static member asset() =
            Arb.fromGen <| gen {
                let! (ContractId contractId) = Arb.generate<ContractId>
                let (ver, cHash) = contractId
                let! (Hash subType) = Arb.generate<Hash>

                return Asset (ver, cHash, subType)
            }

open Arbitrary
open Zen.Crypto

[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<Generators>() |> ignore

[<Property(MaxTest=10000)>]
let ``ContractId created from string should yield expected`` (version : uint32) (Hash hash) =
    let orig = Consensus.Types.ContractId (version, Consensus.Hash.Hash hash)
    let asstring =
        orig.ToString()
        |> System.Text.Encoding.ASCII.GetBytes

    match Zen.ContractId.parse asstring |> unCost with
    | option.Some (ver, cHash) ->
        let tested = Consensus.Types.ContractId (ver, Consensus.Hash.Hash cHash)
        tested = orig
    | _ -> false

[<Property(MaxTest=10000)>]
let ``Asset created from string subtype should yield expected`` (ContractId contractId) (SubtypeString value) =
    let ver, cHash, subType = Zen.Asset.fromSubtypeString contractId value |> unCost
    let expectedVer, expectedCHash = contractId
    let expectedSubtype =
        let bom = [| 0xEFuy; 0xBBuy; 0xBFuy |]
        32 - Array.length value - Array.length bom
        |> Array.zeroCreate
        |> Array.append value
        |> Array.append bom

    ver = expectedVer
    && cHash = expectedCHash
    && subType = expectedSubtype

[<Property(MaxTest=10000)>]
let ``Asset created from int subtype should yield expected`` (ContractId contractId) (i : uint32) =
    let ver, cHash, subType = Zen.Asset.fromSubtypeInt contractId i |> unCost
    let expectedVer, expectedCHash = contractId
    let expectedSubtype =
        let bytes = Infrastructure.BigEndianBitConverter.uint32ToBytes i
        bytes
        |> Array.append (Array.zeroCreate (Consensus.Hash.Length - (Array.length bytes)))

    ver = expectedVer
    && cHash = expectedCHash
    && subType = expectedSubtype

[<Property(MaxTest=10000)>]
let ``Asset encoding round trip should produce same result`` (Asset asset) =
    let ver, cHash, subType = asset
    let contractId = Consensus.Types.ContractId (ver, Consensus.Hash.Hash cHash)
    let asstring =
        (Consensus.Types.Asset (contractId, Consensus.Hash.Hash subType)).ToString()

    let asstring = asstring |> System.Text.Encoding.ASCII.GetBytes


    match Zen.Asset.parse asstring |> unCost with
    | option.Some tested ->
        tested = asset
    | option.None ->
        false

[<Property(MaxTest=10000)>]
let ``Asset encoding round trip should produce same result, using zero sub type`` (ContractId contractId) =
    let ver, cHash = contractId
    let subType = Consensus.Hash.zero |> Consensus.Hash.bytes
    let contractId = Consensus.Types.ContractId (ver, Consensus.Hash.Hash cHash)
    let asstring =
        (Consensus.Types.Asset (contractId, Consensus.Hash.Hash subType)).ToString()
        |> System.Text.Encoding.ASCII.GetBytes

    match Zen.Asset.parse asstring |> unCost with
    | option.Some tested -> tested = (ver, cHash, subType)
    | option.None -> false

[<Property(MaxTest=10000)>]
let ``Asset encoding round trip should produce same result, using Zen asset`` (Hash subType) =
    let zeroHash = Consensus.Hash.zero |> Consensus.Hash.bytes
    let ver, cHash = Consensus.Types.Version0, zeroHash
    let contractId = Consensus.Types.ContractId (ver, Consensus.Hash.Hash cHash)
    let asstring =
        (Consensus.Types.Asset (contractId, Consensus.Hash.Hash subType)).ToString()
        |> System.Text.Encoding.ASCII.GetBytes

    match Zen.Asset.parse asstring |> unCost with
    | option.Some tested -> tested = (ver, cHash, subType)
    | option.None -> false
