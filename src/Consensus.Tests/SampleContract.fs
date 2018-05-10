module Consensus.Tests.SampleContract

open Consensus
open Types
open Hash
open System.Text
open TxSkeleton
open Crypto

let sampleContractCode = """
open Zen.Types
open Zen.Base
open Zen.Cost

module RT = Zen.ResultT
module Tx = Zen.TxSkeleton

let main txSkeleton _ contractHash command sender data wallet =
  let! asset = Zen.Asset.getDefault contractHash in
  let spend = { asset=asset; amount=1000UL } in
  let pInput = Mint spend in

  let! txSkeleton =
    Tx.addInput pInput txSkeleton
    >>= Tx.lockToContract spend.asset spend.amount contractHash in

  RT.ok (txSkeleton, None)

val cf: txSkeleton -> context -> string -> sender -> option data -> wallet -> cost nat 9
let cf _ _ _ _ _ _ = ret (64 + (64 + 64 + 0) + 20)
"""

let sampleContractId = Contract.makeContractId Version0 sampleContractCode

let private sampleContractTester txSkeleton contractId =
    let spend = {
        asset = Asset.defaultOf contractId
        amount = 1000UL
    }

    let output = {
        lock = Lock.Contract contractId
        spend = spend
    }

    let pInput = Mint spend

    let outputs' = txSkeleton.outputs @ [ output ]
    let pInputs' = txSkeleton.pInputs @ [ pInput ]
    { txSkeleton with outputs = outputs'; pInputs = pInputs' }, None

let sampleKeyPair = KeyPair.create()
let samplePrivateKey, samplePublicKey = sampleKeyPair

let sampleInput = {
    txHash = Hash.zero
    index = 1u
}

let sampleOutput = {
    lock = PK (PublicKey.hash samplePublicKey)
    spend = { asset = Asset.defaultOf sampleContractId; amount = 1UL }
}

let sampleInputTx =
    {
        pInputs = [ PointedOutput (sampleInput, sampleOutput) ]
        outputs = [ sampleOutput ]
    }

let sampleOutputTx, _ =
    sampleContractTester sampleInputTx sampleContractId

let sampleExpectedResult =
    Transaction.fromTxSkeleton sampleOutputTx
    |> Transaction.addWitnesses [ ContractWitness <| TxSkeleton.getContractWitness sampleContractId "" None sampleInputTx sampleOutputTx 212L ]
    |> Transaction.sign [ sampleKeyPair ]

let getSampleUtxoset utxos =
    Map.add sampleInput (UtxoSet.Unspent sampleOutput) utxos
