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
let pkKeyLockGenerator = 
    gen {
        let private', public' = KeyPair.create()
        //let! hash = hashGenerator
        return (private', public'), PK (PublicKey.hash public')
    }
let amountGenerator =
    Arb.generate<uint64>
let spendGenerator = 
    gen {
        let! asset = hashGenerator
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