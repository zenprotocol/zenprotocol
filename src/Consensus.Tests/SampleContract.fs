module Consensus.Tests.SampleContract

open Consensus
open Types
open TxSkeleton
open Crypto
open Wallet

let sampleContractCode = """
open Zen.Types
open Zen.Base
open Zen.Cost

module RT = Zen.ResultT
module Tx = Zen.TxSkeleton
module C = Zen.Cost

let main txSkeleton _ contractId command sender messageBody wallet state =
  let! asset = Zen.Asset.getDefault contractId in
  let spend = { asset=asset; amount=1000UL } in
  let pInput = Mint spend in

  let! txSkeleton =
    Tx.addInput pInput txSkeleton
    >>= Tx.lockToContract spend.asset spend.amount contractId in

  RT.ok @ { tx = txSkeleton; message = None; state = NoChange}

let cf _ _ _ _ _ _ _ = 
    64 + (64 + 64 + 0) + 22
    |> cast nat
    |> C.ret
"""

let sampleContractId = Contract.makeContractId Version0 sampleContractCode

let contractWithId = TestWallet.createContractRecord sampleContractCode

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
    |> Transaction.addWitnesses [ ContractWitness <| TxSkeleton.getContractWitness sampleContractId "" None NotCommitted sampleInputTx sampleOutputTx 214L ]
    |> Transaction.sign [ sampleKeyPair ]

let getSampleUtxoset utxos =
    Map.add sampleInput (UtxoSet.Unspent sampleOutput) utxos
