module Consensus.Tests.ContractTests

open Consensus
open NUnit.Framework
open FsUnit
open Hash
open System.Text
open Consensus.Types
open Consensus.TxSkeleton

open TestsInfrastructure.Nunit

let sampleContractCode = """
open Zen.Types
open Zen.Vector
open Zen.Util

val test: transactionSkeleton -> hash -> transactionSkeleton
let test (Tx inputs outputs data) hash =
  let output = {
    lock = ContractLock hash 0 Empty;
    spend = {
      asset = hash;
      amount = 1000UL
    }
  } in

  let input = {
      txHash = hashFromBase64 "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      index = 0ul
  } in

  let outputs' = VCons output outputs in
  let inputs' = VCons input inputs in
  Tx inputs' outputs' data"""

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

    let pInput = ({
        txHash = Hash.zero
        index = 0ul
    }, {
        lock = PK Hash.zero
        spend = { asset = Hash.zero; amount = 0UL }
    })

    let outputs' = output :: txSkeleton.outputs
    let pInputs' = pInput :: txSkeleton.pInputs
    { txSkeleton with outputs = outputs'; pInputs = pInputs' }

let sampleTxSkeleton = 
    {
        pInputs =
            [{
                txHash = Hash.zero
                index = 1u
            }, {
                lock = PK Hash.zero
                spend = { asset = Hash.zero; amount = 1UL }
            }]
        outputs =
            [{
                lock = PK Hash.zero
                spend = { asset = Hash.zero; amount = 1UL }
            }]
    }

let sampleContractExpectedResult = 
    Ok (sampleContractTester sampleTxSkeleton sampleContractHash) : Result<TxSkeleton, string>

let getSampleUtxoset utxos =
    utxos
    |> Map.add {
        txHash = Hash.zero
        index = 1u
    } (UtxoSet.Unspent {
        lock = PK Hash.zero
        spend = { asset = Hash.zero; amount = 1UL }
    })
    |> Map.add {
        txHash = Hash.zero
        index = 0u
    } (UtxoSet.Unspent {
        lock = PK Hash.zero
        spend = { asset = Hash.zero; amount = 0UL }
    })

[<Test>]
let ``Should get contract function``() = 
    match Contract.compile sampleContractCode with
    | Ok contract ->
        // check hash validity
        (Hash.isValid contract.hash, true)
        |> shouldEqual 
        (contract.hash, sampleContractHash)
        |> shouldEqual 

        //execute and check
        let utxos = getSampleUtxoset (UtxoSet.create())
        (Contract.run contract utxos sampleTxSkeleton, sampleContractExpectedResult)
        |> shouldEqual
    | Error err ->
        failwith err