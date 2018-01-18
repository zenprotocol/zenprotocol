module Consensus.Tests.ContractTests

open Consensus
open Types
open NUnit.Framework
open Hash
open System.Text
open TxSkeleton
open Crypto
open SampleContract

open TestsInfrastructure.Nunit

let compileRunAndCompare code =
    code
    |> Contract.compile
    |> Result.bind (fun contract ->
        // check hash validity
        (Hash.isValid contract.hash, true)
        |> shouldEqual 
        (contract.hash, sampleContractHash)
        |> shouldEqual 
        //execute and check
        Contract.run contract sampleInputTx)

[<Test>]
let ``Should get contract function``() = 
    (compileRunAndCompare sampleContractCode
    , (Ok sampleOutputTx : Result<TxSkeleton, string>))
    |> shouldEqual

[<Test>]
let ``Should get 'extract' error for invalid תcode``() = 
    (compileRunAndCompare (sampleContractCode + "###")
    , (Error "extract" : Result<TxSkeleton, string>))
    |> shouldEqual

let validateInputs (contract:Contract.T) utxos tx =
    let acs = ActiveContractSet.add contract.hash contract ActiveContractSet.empty
    TransactionValidation.validateInputs acs utxos (Transaction.hash tx) tx
    |> Result.mapError (function
        | TransactionValidation.ValidationError.General error -> error
        | other -> other.ToString())

let compileRunAndValidate code =
    Contract.compile code
    |> Result.bind (fun contract ->
        let utxoSet = getSampleUtxoset (UtxoSet.create())
        Contract.run contract sampleInputTx
        |> Result.bind (TxSkeleton.checkPrefix sampleInputTx)
        |> Result.map (Transaction.fromTxSkeleton contract.hash)
        |> Result.map (Transaction.addContractWitness contract.hash sampleInputTx)
        |> Result.map (Transaction.sign [ sampleKeyPair ])
        |> Result.bind (validateInputs contract utxoSet))

[<Test>]
let ``Contract generated transaction should be valid``() = 
    (compileRunAndValidate sampleContractCode
    , (Ok sampleExpectedResult : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
let ``Contract should not be able to create not it's own tokens``() = 
    (compileRunAndValidate
         """
         open Zen.Types
         open Zen.Vector
         open Zen.Util

         val test: transactionSkeleton -> hash -> transactionSkeleton
         let test (Tx pInputs outputs data) hash =
           let output = {
             lock = ContractLock hash 0 Empty;
             spend = {
               asset = hashFromBase64 "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; //aha! should be of hash value
               amount = 1000UL
             }
           } in

           let pInput = {
               txHash = hashFromBase64 "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
               index = 0ul
           }, output in

           let outputs' = VCons output outputs in
           let pInputs' = VCons pInput pInputs in
           Tx pInputs' outputs' data"""
    , (Error "illegal creation/destruction of tokens" : Result<Transaction, string>))
    |> shouldEqual