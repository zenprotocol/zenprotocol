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

val cf: txSkeleton -> string -> cost nat 1
let cf _ _ = ret 145

val main: txSkeleton -> hash -> string -> cost (result txSkeleton) 145
let main txSkeleton contractHash command =
  let spend = { asset=contractHash; amount=1000UL } in
  let lock = ContractLock contractHash in

  let output = { lock=lock; spend=spend } in

  let pInput = {
      txHash = hashFromBase64 "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      index = 0ul
  }, output in

  let txSkeleton1 = addInput pInput txSkeleton in
  let txSkeleton2 = txSkeleton1 >>= lockToContract spend contractHash in
  ET.retT txSkeleton2
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
    { txSkeleton with outputs = outputs'; pInputs = pInputs' }

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

let sampleOutputTx =
    sampleContractTester sampleInputTx sampleContractHash

let sampleExpectedResult =
    let tx = Transaction.fromTxSkeleton sampleOutputTx    
    Transaction.addContractWitness sampleContractHash sampleInputTx sampleOutputTx tx
    |> Transaction.sign [ sampleKeyPair ]

let getSampleUtxoset utxos =
    Map.add sampleInput (UtxoSet.Unspent sampleOutput) utxos
