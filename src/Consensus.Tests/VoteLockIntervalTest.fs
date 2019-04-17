module Consensus.Tests.VoteLockIntervalTest

open Consensus
open Crypto
open UtxoSet
open Types
open Hash
open NUnit.Framework
open TransactionNunitHelpers
open TransactionHelpers
open FsUnit
open ValidationError

let outpoint = getInput 1uy 0ul
let privateKey, publicKey = KeyPair.create()
let spend = { asset = Asset.Zen; amount = 10UL }
let lock = Vote ({ allocation = None; payout = None }, 1u, PublicKey.hash publicKey)
let tx =
    {
        version = Version1
        inputs = [ Outpoint outpoint ]
        witnesses = []
        outputs = [ { lock = lock; spend = spend } ]
        contract = None
    }
    |> Transaction.sign [(privateKey, publicKey)] TxHash

let getUTXO _ = UtxoSet.NoOutput
let utxos = Map.ofSeq [ outpoint, Unspent tx.outputs.[0] ] 

let validate interval = 
    let blockNumber = Chain.testParameters.intervalLength * interval + 1ul
    TransactionValidation.validateInContext Chain.localParameters getUTXO contractPath blockNumber 0UL ActiveContractSet.empty Map.empty utxos getContractState ContractStates.asDatabase (Transaction.toExtended tx)

let expectOk interval = 
    validate interval
    |> function
        | Error error -> 
            failwithf "Expected Ok but was %A" error
        | Ok _ ->
            ()
    
[<Test>]
let ``Should validate a transaction with voting lock having correct interval``() =
    expectOk 1ul

[<Test>]
let ``Should validate a transaction with voting lock having past interval``() =
    expectOk 0ul

[<Test>]
let ``Should validate a transaction with voting lock having future interval``() =
    expectOk 2ul
