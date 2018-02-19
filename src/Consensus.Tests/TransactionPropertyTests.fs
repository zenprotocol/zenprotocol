module Consensus.Tests.TransactionPropertyTests

open Consensus
open Consensus.TxSkeleton
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Crypto
open TransactionValidation

open TransactionFsCheckHelpers
open TransactionHelpers
open TestsInfrastructure.FsCheck

type PkTransaction = {
    tx : Transaction
    utxos: UtxoSet.T
    keys: KeyPair list
}

type ArbitraryGenerators =
    static member PublicKeyUtxoSetGenerator() =
        let utxoSetGenerator =
            gen {
                let! size = Gen.choose (1,4)
                let! pkKeyOutputs = Gen.listOfLength size pkKeyOutputGenerator
                let pkOutputs = pkKeyOutputs |> List.map fst
                let unspentPkOutputs = pkOutputs |> List.map Unspent
                let outpoint i =
                    {
                        txHash = Array.create 32 i |> Hash
                        index = uint32 i
                    }
                let outpoints = [ 1uy..List.length pkOutputs |> byte ] |> List.map outpoint
                return List.zip outpoints unspentPkOutputs |> Map.ofList, Map.ofList pkKeyOutputs
            }
        let txGenerator (utxos, (keyMap: Map<Output, KeyPair>)) =
            let getUTXO _ = UtxoSet.NoOutput

            gen {
                let outpoints = utxos |> Map.toList |> List.map fst
                let! txInputs = Gen.shuffle outpoints
                let! size = Gen.choose (0, Array.length txInputs - 1)
                let txInputs = List.ofArray txInputs.[0..size]
                let txOutputs = UtxoSet.getUtxos getUTXO txInputs utxos |> Option.get //expecting Some
                let keys = List.map (fun output -> Map.find output keyMap) txOutputs
                let tx =
                    {
                        inputs = List.map Outpoint txInputs
                        outputs = txOutputs
                        witnesses = []
                        contract = None
                    }

                return {
                    tx = tx
                    utxos = utxos
                    keys = keys
                }
            }
        Arb.fromGen (utxoSetGenerator >>= txGenerator)

let acs = ActiveContractSet.empty

let getSignedTx tx keys =
    let signedTx = Transaction.sign keys tx
    let txHash = Transaction.hash signedTx

    signedTx, txHash


[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<ArbitraryGenerators>() |> ignore

[<Property>]
let ``Transaction should be valid`` ({ utxos = utxos; tx = tx; keys = keys })=
    inputsValidationOk 1ul acs utxos tx keys
    |> shouldEqual

[<Property>]
let ``Transaction should have invalid amounts`` ({ utxos = utxos; tx = tx; keys = keys }) =
    match tx.outputs with
    | outputsHead :: outputsTail ->
        let outputsHead = { outputsHead with spend = { outputsHead.spend with amount = outputsHead.spend.amount - 1UL } }
        let tx = { tx with outputs = outputsHead :: outputsTail }
        inputsValidationMsg "invalid amounts" 1ul acs utxos tx keys
        |> shouldEqual
    | _ -> true

[<Property>]
let ``Transaction validation should fail with inputs empty error`` (tx:Transaction) =
    basicValidationMsg "inputs empty" { tx with inputs = List.empty }
    |> shouldEqual

[<Property>]
let ``Transaction validation should fail with outputs empty error`` (tx:Transaction) =
    not <| List.isEmpty tx.inputs ==> lazy (
        basicValidationMsg "outputs empty" { tx with outputs = List.empty }
        |> shouldEqual
    )