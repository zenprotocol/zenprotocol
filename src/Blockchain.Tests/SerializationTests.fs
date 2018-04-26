module Blockchain.Tests.SerializationTests

open Consensus
open Hash
open Types
open Transaction
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Serialization
open FsUnit
open Blockchain
open Serialization
open Consensus.Tests
open Blockchain.ExtendedBlockHeader

type BlockchainGenerators =
    static member BlockState() =
        gen {
            let! ema = Arb.generate<EMA.T>
            let! activeContractSet =
                Gen.listOf <| gen {
                    let! hash = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                    let! blockNumber = Arb.generate<uint32>
                    let! code = Arb.generate<string> |> Gen.filter ((<>) null)
                    return Hash hash, blockNumber, code
                }
                
            return
                ({
                    ema = ema
                    activeContractSet = activeContractSet
                } : BlockState.T)
        }
        |> Arb.fromGen

    static member BlockHeader() =
        gen {
            let! version = Arb.generate<uint32>
            let! parent = Gen.arrayOfLength Hash.Length Arb.generate<byte>
            let! blockNumber = Arb.generate<uint32>
            let! commitments = Gen.arrayOfLength Hash.Length Arb.generate<byte>
            let! timestamp = Arb.generate<uint64>
            let! difficulty = Arb.generate<uint32>
            let! nonceFst = Arb.generate<uint64>
            let! nonceSnd = Arb.generate<uint64>

            return {
                version = version
                parent = Hash parent
                blockNumber = blockNumber
                commitments = Hash commitments
                timestamp = timestamp
                difficulty = difficulty
                nonce = nonceFst, nonceSnd
            }
        }
        |> Arb.fromGen

    static member ExtendedBlockHeader() =
        gen {
            let! hash = Gen.arrayOfLength Hash.Length Arb.generate<byte>
            let! header = Arb.generate<BlockHeader>
            let! status = Arb.generate<BlockStatus>
            let! chainWork = Arb.generate<bigint option> |> Gen.filter (function | None -> true | Some i -> i > 0I)
            let! txMerkleRoot = Gen.arrayOfLength Hash.Length Arb.generate<byte>
            let! witnessMerkleRoot = Gen.arrayOfLength Hash.Length Arb.generate<byte>
            let! activeContractSetMerkleRoot = Gen.arrayOfLength Hash.Length Arb.generate<byte>
            let! commitments = Gen.listOf <| gen {
                let! hash = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                return Hash hash
            }
            
            return {
                hash = Hash hash
                header = header
                status = status
                chainWork = chainWork
                txMerkleRoot = Hash txMerkleRoot
                witnessMerkleRoot = Hash witnessMerkleRoot
                activeContractSetMerkleRoot = Hash activeContractSetMerkleRoot
                commitments = commitments
            }
        }
        |> Arb.fromGen
    
[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<ConsensusGenerator>() |> ignore
    Arb.register<BlockchainGenerators>() |> ignore

[<Property>]
let ``BlockState serialization round trip produces same result`` (value:BlockState.T) =
    value
    |> BlockState.serialize
    |> BlockState.deserialize = value

[<Property>]
let ``ExtendedBlockHeader serialization round trip produces same result`` (value:ExtendedBlockHeader.T) =
    value
    |> ExtendedBlockHeader.serialize
    |> ExtendedBlockHeader.deserialize = value

[<Property>]
let ``OutputStatus serialization round trip produces same result`` (value:UtxoSet.OutputStatus) =
    value
    |> OutputStatus.serialize
    |> OutputStatus.deserialize = value

[<Property>]
let ``PointedOutput serialization round trip produces same result`` (value:PointedOutput) =
    value
    |> PointedOutput.serialize
    |> PointedOutput.deserialize = value

[<Property>]
let ``Version serialization round trip produces same result`` (value:int) =
    value
    |> Version.serialize
    |> Version.deserialize = value