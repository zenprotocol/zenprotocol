module Wallet.Tests.AddressTests

open NUnit.Framework
open Consensus
open Consensus.Types
open Chain
open Address
open Hash
open FsCheck.NUnit
open FsCheck
open TestsInfrastructure.FsCheck

type ValidHash = ValidHash of Hash.Hash

type ArbitraryGenerators =
    static member ValidHashGenerator() =
        Arb.fromGen (gen {
            let! bytes = Gen.arrayOfLength 32 Arb.generate<byte>
            return ValidHash (Hash.Hash bytes)
        })

[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<ArbitraryGenerators>() |> ignore

let msg msg =
    Error msg : Result<Hash, string>

let ok hash =
    Ok hash : Result<Hash, string>

let okC : ContractId -> Result<ContractId, string> = Ok

let msgC msg =
    Error msg : Result<ContractId, string>

[<Property>]
let ``Contract address encode-decode roundtrip should produce same result``(chain:Chain) (version:uint32) (ValidHash hash) =
    let address = Address.encode chain (Contract <| ContractId  (version,hash))
    (Address.decodeContract chain address, okC <| ContractId (version,hash))
    |> shouldEqual

[<Property>]
let ``PK address encode-decode roundtrip should produce same result``(chain:Chain) (ValidHash hash) =
    let address = Address.encode chain (PK hash)
    (Address.decodePK chain address, ok hash)
    |> shouldEqual

[<Property>]
let ``Decoding Contract address of PK encoded address results in error``(chain:Chain) (ValidHash hash) =
    let address = Address.encode chain (PK hash)
    (Address.decodeContract chain address, msgC "address type mismatch, Contract expected")
    |> shouldEqual

[<Property>]
let ``Decoding PK address of Contract encoded address results in error``(chain:Chain) version (ValidHash hash) =
    let address = Address.encode chain (Contract <| ContractId (version,hash))
    (Address.decodePK chain address, msg "address type mismatch, Public Key expected")
    |> shouldEqual
