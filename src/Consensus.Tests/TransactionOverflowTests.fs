module Consensus.Tests.TransactionOverflowTests

open System
open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open NUnit.Framework
open TransactionNunitHelpers
open TransactionHelpers
open Crypto

open TransactionNunitHelpers
open TransactionHelpers
open TestsInfrastructure.Nunit

let utxos = UtxoSet.asDatabase
let acs = ActiveContractSet.empty

[<Test>]
let ``Should produce outputs overflow error``() =
    let keys = getKeys 1
    let input = getInput 1uy 0ul

    let tx = {
        version = Version0
        inputs = [ Outpoint input ];
        witnesses = []
        outputs =
            [
                { lock = PK testHash; spend = { asset = Asset.Zen; amount = UInt64.MaxValue } };
                { lock = PK testHash; spend = { asset = Asset.Zen; amount = 1UL } }
            ]
        contract = None
    }

    let utxos = addUtxo input keys.[0] 1UL utxos
    inputsValidationMsg "outputs overflow" 1ul acs utxos tx keys
    |> shouldEqual

[<Test>]
let ``Should produce inputs overflow error``() =
    let keys = getKeys 2
    let input1, input2 = getInput 1uy 0ul, getInput 2uy 0ul

    let tx = {
        version = Version0
        inputs = [ Outpoint input1; Outpoint input2 ]
        witnesses = []
        outputs =
            [
                { lock = PK testHash; spend = { asset = Asset.Zen; amount = 1UL } }
            ]
        contract = None
    }

    let utxos = addUtxo input1 keys.[0] 1UL utxos
    let utxos = addUtxo input2 keys.[1] UInt64.MaxValue utxos
    inputsValidationMsg "inputs overflow" 1ul acs utxos tx keys
    |> shouldEqual
