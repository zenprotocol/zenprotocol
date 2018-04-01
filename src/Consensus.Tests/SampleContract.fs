module Consensus.Tests.SampleContract

open Consensus
open Types
open Hash
open System.Text
open TxSkeleton
open Crypto

let sampleContractCode = """
open Zen.Types
open Zen.Vector
open Zen.Base
open Zen.Cost

module ET = Zen.ErrorT
module Tx = Zen.TxSkeleton

val main: txSkeleton -> hash -> string -> data -> option lock -> wallet
    -> result (txSkeleton ** option message) `cost` (64 + (64 + 64 + 0) + 23)
let main txSkeleton contractHash command data returnAddress wallet =
  let! asset = Zen.Asset.getDefault contractHash in
  let spend = { asset=asset; amount=1000UL } in
  let lock = ContractLock contractHash in

  let output = { lock=lock; spend=spend } in
  let pInput = Mint spend in

  let! txSkeleton =
    Tx.addInput pInput txSkeleton
    >>= Tx.lockToContract spend.asset spend.amount contractHash in

  ET.ret (txSkeleton, None)

val cf: txSkeleton -> string -> data -> option lock -> wallet -> cost nat 9
let cf _ _ _ _ _ = ret (64 + (64 + 64 + 0) + 23)
"""

let sampleContractHash =
    sampleContractCode
    |> Encoding.UTF8.GetBytes
    |> Hash.compute

let private sampleContractTester txSkeleton hash =
    let spend = {
        asset = hash, Hash.zero
        amount = 1000UL
    }

    let output = {
        lock = Lock.Contract hash
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
    spend = { asset = sampleContractHash, Hash.zero; amount = 1UL }
}

let sampleInputTx =
    {
        pInputs = [ PointedOutput (sampleInput, sampleOutput) ]
        outputs = [ sampleOutput ]
    }

let sampleOutputTx, _ =
    sampleContractTester sampleInputTx sampleContractHash

let sampleExpectedResult =
    Transaction.fromTxSkeleton sampleOutputTx
    |> Transaction.addWitnesses [ ContractWitness <| TxSkeleton.getContractWitness sampleContractHash "" Contract.EmptyData (PK Hash.zero) sampleInputTx sampleOutputTx 215L ]
    |> Transaction.sign [ sampleKeyPair ]

let getSampleUtxoset utxos =
    Map.add sampleInput (UtxoSet.Unspent sampleOutput) utxos
