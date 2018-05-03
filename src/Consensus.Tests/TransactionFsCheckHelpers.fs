module Consensus.Tests.TransactionFsCheckHelpers

open Consensus
open Consensus.Types
open Consensus.Hash
open FsCheck
open Hash
open Crypto

let hashGenerator =
    gen {
        let! hash = Gen.arrayOfLength Hash.Length Arb.generate<byte>
        return hash |> Hash
    }
let assetGenerator =
    gen {
        let! contractHash = Gen.arrayOfLength Hash.Length Arb.generate<byte>
        let! tokenHash = Gen.arrayOfLength Hash.Length Arb.generate<byte>
        return Asset (ContractId (Version0, Hash contractHash), Hash tokenHash)
    }
let pkKeyLockGenerator =
    gen {
        let private', public' = KeyPair.create()
        return (private', public'), PK (PublicKey.hash public')
    }
let amountGenerator =
    Arb.generate<uint64> |> Gen.filter ((<>) 0UL)
let spendGenerator =
    gen {
        let! asset = assetGenerator
        let! amount = amountGenerator
        return { asset = asset; amount = amount }
    }
let pkKeyOutputGenerator =
    gen {
        let! pkKeyLock = pkKeyLockGenerator
        let! spend = spendGenerator
        return { lock = snd pkKeyLock; spend = spend}, fst pkKeyLock
    }
let indexGenerator =
    Arb.generate<uint32>
let outpointGenerator =
    gen {
        let! txHash = hashGenerator
        let! index = indexGenerator
        return { txHash = txHash; index = index;}
    }