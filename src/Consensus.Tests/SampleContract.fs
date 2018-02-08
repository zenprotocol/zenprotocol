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
open Zen.Util
open Zen.Base
open Zen.Cost

module ET = Zen.ErrorT
module Tx = Zen.TxSkeleton

val cf: txSkeleton -> string -> option lock -> #l:nat -> wallet l -> cost nat 7
let cf _ _ _ #l _ = ret (64 + 64 + 0 + 23)

val main: txSkeleton -> hash -> string -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message)) (64 + 64 + 0 + 23)
let main txSkeleton contractHash command returnAddress #l wallet =
  let spend = { asset=contractHash; amount=1000UL } in
  let lock = ContractLock contractHash in

  let output = { lock=lock; spend=spend } in

  let pInput = {
      txHash = hashFromBase64 "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      index = 0ul
  }, output in

  let! txSkeleton =
    Tx.addInput pInput txSkeleton
    >>= Tx.lockToContract spend.asset spend.amount contractHash in

  ET.ret (txSkeleton, None)
  """

let sampleContractHash =
    sampleContractCode
    |> Encoding.UTF8.GetBytes
    |> Hash.compute

let private sampleContractTester txSkeleton hash =
    let output = {
        lock = Lock.Contract hash
        spend =
        {
            asset = hash
            amount = 1000UL
        }
    }

    let pInput =
        {
            txHash = Hash.zero
            index = 0ul
        }, output

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
    spend = { asset = Hash.zero; amount = 1UL }
}

let sampleInputTx =
    {
        pInputs = [ sampleInput, sampleOutput ]
        outputs = [ sampleOutput ]
    }

let sampleOutputTx, _ =
    sampleContractTester sampleInputTx sampleContractHash

let sampleExpectedResult =
    Transaction.fromTxSkeleton sampleOutputTx
    |> Transaction.addWitnesses [ TxSkeleton.getContractWitness sampleContractHash "" (PK Hash.zero) sampleInputTx sampleOutputTx ]
    |> Transaction.sign [ sampleKeyPair ]

let getSampleUtxoset utxos =
    Map.add sampleInput (UtxoSet.Unspent sampleOutput) utxos
