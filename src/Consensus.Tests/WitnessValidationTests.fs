module Consensus.Tests.WitnessValidationTests

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open NUnit.Framework
open Crypto
open TransactionValidation

open TransactionNunitHelpers
open TransactionHelpers
open TestsInfrastructure.Nunit

let acs = ActiveContractSet.empty

let testInput1 = getInput 1uy 0ul
let testInput2 = getInput 2uy 0ul
let keys = getKeys 1


[<Test>]
let ``Signed transaction validation result should be invalid witness``() =
    let outputLock = PK testHash // testHash will not match keypair
    let output = { lock = outputLock; spend = { asset = Hash.zero; amount = 1UL } }
    let tx = {
        inputs = [ testInput1 ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ testInput1, Unspent output ]
    inputsValidationMsg "PK witness mismatch" acs utxos tx keys
    |> shouldEqual
