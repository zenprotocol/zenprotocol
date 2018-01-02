module Consensus.Tests.ContractTests

open Consensus
open NUnit.Framework
open FsUnit
open Hash
open System.Text
open Consensus.Types
open Consensus.TxSkeleton

let sampleContractCode = """
open Zen.Types
open Zen.Vector

val test: transactionSkeleton -> hash -> transactionSkeleton
let test (Tx inputs outputs data) hash =
  let output = {
    lock = ContractLock hash 0 Empty;
    spend = {
      asset = hash;
      amount = 1000UL
    }
  } in

  let outputs' = VCons output outputs in
  Tx inputs outputs' data"""

let sampleContractHash = 
    sampleContractCode
    |> Encoding.ASCII.GetBytes
    |> Hash.compute

let private sampleContractTester txSkeleton hash =
    let output = {
        lock = Lock.Contract (hash, [||])
        spend =
        {
            asset = hash
            amount = 1000UL
        }
    } 

    let outputs' = output :: txSkeleton.outputs
    { txSkeleton with outputs = outputs' }

let sampleTxSkeleton = 
    {
        inputs =
            [{
                txHash = Hash.zero
                index = 1u
            }]
        outputs =
            [{
                lock = Lock.PK Hash.zero
                spend = { asset = Hash.zero; amount = 1UL }
            }]
    }

let sampleContractExpectedResult = 
    Ok (sampleContractTester sampleTxSkeleton sampleContractHash) : Result<TxSkeleton, string>

[<Test>]
let ``Should get contract function``() = 
    match Contract.compile sampleContractCode with
        | Ok contract ->
            // check hash validity
            Hash.isValid contract.hash |> should equal true 
            contract.hash 
            |> should equal sampleContractHash
            //execute and check
            Contract.run contract sampleTxSkeleton
            |> should equal sampleContractExpectedResult
        | Error err ->
            failwith err