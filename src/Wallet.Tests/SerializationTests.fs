module Wallet.Tests.SerializationTests

open Consensus
open Consensus.Serialization.Serialization
open Consensus.Tests
open Hash
open Types
open Transaction
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Wallet
open FsUnit
open Infrastructure.Result
open Serialization
open Wallet.Types


type WalletGenerators =

    static member ExtendedPublicKeyGenerator() =
        gen {
            let! seed = Gen.arrayOfLength 64 Arb.generate<byte>
            let publicKey =
                ExtendedKey.create seed
                >>= ExtendedKey.neuter
                |> get

            return publicKey
        }

    static member OutputGenerator() =
        gen {
            let! pkHash = ConsensusGenerator.HashGenerator().Generator
            let! spend = Arb.generate<Spend>
            let lock = PK pkHash
            let! outpoint = Arb.generate<Outpoint>
            let! status = Arb.generate<Status>
            let! confirmationStatus = Arb.generate<ConfirmationStatus>

            return {
                pkHash=pkHash
                spend=spend
                lock=lock
                outpoint=outpoint
                status=status
                confirmationStatus=confirmationStatus
            }
        }
        |> Arb.fromGen

    static member AccountGenerator() =
        gen {
            let! blockNumber = Arb.generate<uint32>
            let! blockHash = ConsensusGenerator.HashGenerator().Generator
            let! counter = Arb.generate<int32>
            let! publicKey = WalletGenerators.ExtendedPublicKeyGenerator()
            let! secure = Gen.arrayOf Arb.generate<byte>
            let! externalPkHash = ConsensusGenerator.HashGenerator().Generator
            let! changePKHash = ConsensusGenerator.HashGenerator().Generator

            return
                {
                    blockNumber=blockNumber
                    blockHash=blockHash
                    counter=counter
                    publicKey=publicKey
                    secureMnemonicPhrase=secure
                    externalPKHash=externalPkHash
                    changePKHash=changePKHash
                }
        }
        |> Arb.fromGen

[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<Consensus.Tests.ConsensusGenerator>() |> ignore
    Arb.register<WalletGenerators>() |> ignore

[<Property(EndSize=10000,MaxTest=1000)>]
let ``Account serialization round trip produces same result`` (value:Account) =
    value
    |> Account.serialize
    |> Account.deserialize = Some value

[<Property(EndSize=10000,MaxTest=1000)>]
let ``Address serialization round trip produces same result`` (value:Address) =
    value
    |> Address.serialize
    |> Address.deserialize = Some value

[<Property(EndSize=10000,MaxTest=1000)>]
let ``Output serialization round trip produces same result`` (value:Output) =
    value
    |> Output.serialize
    |> Output.deserialize = Some value
