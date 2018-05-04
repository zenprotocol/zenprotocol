module Consensus.Tests.TransactionNunitHelpers

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open Crypto

let testHash = Hash (Array.create 32 1uy)

let getInput v idx =
    {
        txHash = Hash (Array.create 32 v)
        index = idx
    }

let getKeys n =
    [1..n]
    |> List.map (fun _ -> KeyPair.create())

let private getPkLock (_, publicKey) =
    PK (PublicKey.hash publicKey)

let addUtxo input ketPair amount =
    let output = Unspent {
        lock = getPkLock ketPair
        spend =
        {
            asset = Asset.Zen
            amount = amount
        }
    }
    Map.add input output

//open TestsInfrastructure.Nunit
//open TransactionHelpers

//let inputsValidation expected acs utxos tx keys =
    //inputsValidation expected acs utxos tx keys >> shouldEqual
