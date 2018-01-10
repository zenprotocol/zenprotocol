module Wallet.Tests.AddressTests

open NUnit.Framework
open Consensus
open ChainParameters
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

[<Property>]
let ``Contract address encode-decode roundtrip should produce same result``(chain:Chain) (ValidHash hash) =
    let address = Address.encode (Contract hash) chain 
    (Address.decodeContract address chain, ok hash)
    |> shouldEqual

[<Property>]
let ``PK address encode-decode roundtrip should produce same result``(chain:Chain) (ValidHash hash) =
    let address = Address.encode (PK hash) chain 
    (Address.decodePK address chain, ok hash)
    |> shouldEqual

[<Property>]
let ``Decoding Contract address of PK encoded address results in error``(chain:Chain) (ValidHash hash) =
    let address = Address.encode (PK hash) chain 
    (Address.decodeContract address chain, msg "address type discriminator mismatch, Contract expected")
    |> shouldEqual

[<Property>]
let ``Decoding PK address of Contract encoded address results in error``(chain:Chain) (ValidHash hash) =
    let address = Address.encode (Contract hash) chain 
    (Address.decodePK address chain, msg "address type discriminator mismatch, Public Key expected")
    |> shouldEqual
