module Wallet.Tests.SerializationTests

open Consensus
open Consensus.Serialization.Serialization
open Hash
open Types
open Transaction
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Wallet
open FsUnit
open Serialization
open Wallet.Account

type WalletGenerators =
    static member HashGenerator() =
        gen {
            let! bytes = Gen.arrayOfLength Hash.Length Arb.generate<byte>
            return Hash bytes
        }
    static member PublicKeyGenerator() =
        gen {
            let _, publicKey = Crypto.KeyPair.create()
            return publicKey
        }
    static member TxDeltaGenerator() =
        gen {
            let! txHash = WalletGenerators.HashGenerator()
            let! deltas = Arb.generate<List<SpendStatus>>
            let! blockNumber = Arb.generate<Option<uint32>> 

            return
                {
                    txHash = txHash
                    deltas = deltas
                    blockNumber = blockNumber
                }
        }
        |> Arb.fromGen

    static member WalletGenerator() =
        gen {
            let! deltas = Arb.generate<List<TxDelta>>
            let! outputs = Arb.generate<Map<Outpoint, OutputStatus>>
            let! transactions = Gen.listOf <| Consensus.Tests.ConsensusGenerator.Transaction().Generator
            let transactions = List.map (fun tx -> { tx with witnesses = [] }) transactions
            let txHashes = List.map Transaction.hash transactions
            let mempool = List.zip txHashes transactions
            let! tip = WalletGenerators.HashGenerator()
            let! blockNumber = Arb.generate<uint32> 
            let! publicKey = WalletGenerators.PublicKeyGenerator()
            
            return
                {
                    deltas = deltas
                    outputs = outputs
                    mempool = mempool
                    tip = tip
                    blockNumber = blockNumber
                    publicKey = publicKey
                }            
        }
        |> Arb.fromGen
    
[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<Consensus.Tests.ConsensusGenerator>() |> ignore
    Arb.register<WalletGenerators>() |> ignore

[<Property(EndSize=10000)>]
let ``Wallet serialization round trip produces same result`` (value:Account.T) =
    value
    |> Wallet.serialize
    |> Wallet.deserialize = Some value