module Wallet.Tests.AddressTests

open NUnit.Framework
open Consensus
open ChainParameters
open Address
open Hash
open FsCheck.NUnit
open FsCheck
open FsUnit

let shouldEqual expected found =
    try 
        should equal expected found
    with _ as ex ->
        printfn "expected: %A" expected
        printfn "   found: %A" found
        raise ex

type Arbitrary = 
    | ValidHash of Hash.Hash 

type ArbitraryGenerators = 
    static member ValidHashGenerator() = 
        Arb.fromGen (gen {
            let! bytes = Gen.arrayOfLength 32 Arb.generate<byte>    
            return ValidHash (Hash.Hash bytes)
        })

[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<ArbitraryGenerators>() |> ignore

[<Property>]
let ``Contract address encode-decode roundtrip should produce same result``(chain:Chain) (ValidHash hash) =
    let address = Address.encode (Contract hash) chain 
    Address.decodeContract address chain
    |> shouldEqual (Ok hash : Result<Hash, string>)

[<Property>]
let ``PK address encode-decode roundtrip should produce same result``(chain:Chain) (ValidHash hash) =
    let address = Address.encode (PK hash) chain 
    Address.decodePK address chain
    |> shouldEqual (Ok hash : Result<Hash, string>)

[<Property>]
let ``Decoding Contract address of PK encoded address results in error``(chain:Chain) (ValidHash hash) =
    let address = Address.encode (PK hash) chain 
    Address.decodeContract address chain
    |> shouldEqual (Error "address type discriminator mismatch, Contract expected" : Result<Hash, string>)

[<Property>]
let ``Decoding PK address of Contract encoded address results in error``(chain:Chain) (ValidHash hash) =
    let address = Address.encode (Contract hash) chain 
    Address.decodePK address chain
    |> shouldEqual (Error "address type discriminator mismatch, Public Key expected" : Result<Hash, string>)
