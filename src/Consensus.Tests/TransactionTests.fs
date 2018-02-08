module Consensus.Tests.TransactionTests

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
let ``Transaction should be orphan``() =
    let orphanInput = {
        txHash = Hash (Array.create 32 100uy)
        index = 0ul
    }
    let output = { lock = PK testHash; spend = { asset = Hash.zero; amount = 1UL } }
    let tx = {
        inputs = [ testInput1; orphanInput ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ testInput1, Unspent output ]
    inputsValidationOrphan 1ul acs utxos tx keys
    |> shouldEqual

[<Test>]
let ``Transaction basic validation should be Ok``() =
    let output = { lock = PK testHash; spend = { asset = Hash.zero; amount = 1UL } }
    let tx = {  
        inputs = [ testInput1 ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    basicValidationOk tx
    |> shouldEqual

[<Test>]
let ``Transaction validation should fail with outputs invalid error``() =
    let tx = {
        inputs = [ testInput1 ];
        witnesses = []
        outputs = [ { lock = PK testHash; spend = { asset = Hash.zero; amount = 0UL } } ]
        contract = None
    }
    basicValidationMsg "outputs invalid" tx
    |> shouldEqual

[<Test>]
let ``Transaction validation should fail with duplicate inputs error``() =
    let tx = {
        inputs = [ testInput1; testInput1 ]
        witnesses = []
        outputs = [ { lock = PK testHash; spend = { asset = Hash.zero; amount = 1UL } } ]
        contract = None
    }
    basicValidationMsg "inputs duplicated" tx
    |> shouldEqual

[<Test>]
let ``Transaction validation should fail with inputs structurally invalid error``() =
    let invalidHash = Hash (Array.create 31 1uy) // an invalid hash with length 31
    let tx = {
        inputs = [ { txHash = invalidHash; index = 0ul } ]
        witnesses = []
        outputs = [ { lock = PK testHash; spend = { asset = Hash.zero; amount = 1UL } } ]
        contract = None
    }
    basicValidationMsg "inputs structurally invalid" tx
    |> shouldEqual

[<Test>]
let ``Signed transaction should be valid``() =
    let _, publicKey = keys.[0]
    let outputLock = PK (PublicKey.hash publicKey)
    let output = { lock = outputLock; spend = { asset = Hash.zero; amount = 1UL } }
    let tx = {
        inputs = [ testInput1 ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ testInput1, Unspent output ]
    inputsValidationOk 1ul acs utxos tx keys
    |> shouldEqual

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
    inputsValidationMsg "PK witness mismatch" 1ul acs utxos tx keys
    |> shouldEqual 