module Consensus.Tests.ContractTests

open Consensus
open Types
open NUnit.Framework
open Hash
open System.Text
open TxSkeleton
open Crypto
open SampleContract

open Consensus
open TestsInfrastructure.Nunit

let contractsPath = "./test"

let getUTXO _ = UtxoSet.NoOuput
let getWallet _ = Map.empty

let compileRunAndCompare code =
    Contract.recordHints code
    |> Result.map (fun hints -> (code, hints))
    |> Result.bind (Contract.compile contractsPath)
    |> Result.bind (fun contract ->
        // check hash validity
        (Hash.isValid contract.hash, true)
        |> shouldEqual
        (contract.hash, sampleContractHash)
        |> shouldEqual
        //execute and check
        Contract.run contract "" Map.empty sampleInputTx)

[<Test>]
let ``Should get contract function``() =
    (compileRunAndCompare sampleContractCode
    , (Ok sampleOutputTx : Result<TxSkeleton, string>))
    |> shouldEqual

[<Test>]
let ``Should get 'elaborate' error for invalid תcode``() =
    (compileRunAndCompare (sampleContractCode + "###")
    , (Error "elaborate" : Result<TxSkeleton, string>))
    |> shouldEqual

let validateInputs (contract:Contract.T) utxos contractWallets tx  =
    let acs = ActiveContractSet.add contract.hash contract ActiveContractSet.empty
    TransactionValidation.validateInputs getUTXO getWallet acs utxos contractWallets (Transaction.hash tx) tx
    |> Result.mapError (function
        | TransactionValidation.ValidationError.General error -> error
        | other -> other.ToString())

let compileRunAndValidate code =
    Contract.recordHints code
    |> Result.map (fun hints -> (code, hints))
    |> Result.bind (Contract.compile contractsPath)
    |> Result.bind (fun contract ->

        let utxoSet = getSampleUtxoset (UtxoSet.asDatabase)
        Contract.run contract "" Map.empty sampleInputTx
        |> Result.bind (TxSkeleton.checkPrefix sampleInputTx)
        |> Result.map (fun finalTxSkeleton ->
            let tx = Transaction.fromTxSkeleton finalTxSkeleton
            Transaction.addContractWitness contract.hash sampleInputTx finalTxSkeleton tx)        
        |> Result.map (Transaction.sign [ sampleKeyPair ])
        |> Result.bind (validateInputs contract utxoSet ContractWallets.asDatabase)
        |> Result.map fst)

[<Test>]
let ``Contract generated transaction should be valid``() =
    (compileRunAndValidate sampleContractCode
    , (Ok sampleExpectedResult : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
let ``Contract should not be able to create tokens other than its own``() =
    (compileRunAndValidate
         """
         open Zen.Types
         open Zen.Vector
         open Zen.Util
         open Zen.Base
         open Zen.Cost

         module ET = Zen.ErrorT

         val cf: txSkeleton -> string -> cost nat 1
         let cf _ _ = ret 149

         val main: txSkeleton -> hash -> string -> cost (result txSkeleton) 149
         let main txSkeleton contractHash command =
           let spend = {
               asset=hashFromBase64 "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
               amount=1000UL
               } in
           let lock = ContractLock contractHash in

           let output = { lock=lock; spend=spend } in

           let pInput = {
               txHash = hashFromBase64 "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
               index = 0ul
           }, output in

           let txSkeleton1 = addInput pInput txSkeleton in
           let txSkeleton2 = txSkeleton1 >>= lockToContract spend.asset spend.amount contractHash in
           ET.retT txSkeleton2
           """
    , (Error "illegal creation/destruction of tokens" : Result<Transaction, string>))
    |> shouldEqual
