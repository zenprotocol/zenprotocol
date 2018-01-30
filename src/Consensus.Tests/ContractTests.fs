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

let getUTXO _ = UtxoSet.NoOutput

let compile code = 
    Contract.recordHints code
    |> Result.map (fun hints -> (code, hints))
    |> Result.bind (Contract.compile contractsPath)

let compileRunAndCompare code =
    compile code
    |> Result.bind (fun contract ->
        // check hash validity
        (Hash.isValid contract.hash, true)
        |> shouldEqual
        (contract.hash, sampleContractHash)
        |> shouldEqual
        Contract.run contract "" List.empty sampleInputTx)

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

let validateInputs (contract:Contract.T) utxos tx  =
    let acs = ActiveContractSet.add contract.hash contract ActiveContractSet.empty
    TransactionValidation.validateInputs getUTXO acs utxos (Transaction.hash tx) tx
    |> Result.mapError (function
        | TransactionValidation.ValidationError.General error -> error
        | other -> other.ToString())

let compileRunAndValidate code =
    compile code
    |> Result.bind (fun contract ->
        let utxoSet = getSampleUtxoset (UtxoSet.asDatabase)
        Contract.run contract "" List.empty sampleInputTx
        |> Result.bind (TxSkeleton.checkPrefix sampleInputTx)
        |> Result.map (fun finalTxSkeleton ->
            let tx = Transaction.fromTxSkeleton finalTxSkeleton
            Transaction.addContractWitness contract.hash "" sampleInputTx finalTxSkeleton tx)        
        |> Result.map (Transaction.sign [ sampleKeyPair ])
        |> Result.bind (validateInputs contract utxoSet))        

[<Test>]
let ``Contract generated transaction should be valid``() =
    (compileRunAndValidate sampleContractCode
    , (Ok sampleExpectedResult : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
let ``Should get expected contract cost``() =
    (compile sampleContractCode
     |> Result.bind (fun contract -> 
        Contract.getCost contract "" List.empty sampleInputTx)
    , (Ok 146I : Result<bigint, string>))
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
         module Tx = Zen.TxSkeleton

         val cf: txSkeleton -> string -> #l:nat -> wallet l -> cost nat 1
         let cf _ _ #l _ = ret 149

         val main: txSkeleton -> hash -> string -> #l:nat -> wallet l -> cost (result txSkeleton) 149
         let main txSkeleton contractHash command #l wallet =
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

           let txSkeleton1 = Tx.addInput pInput txSkeleton in
           let txSkeleton2 = txSkeleton1 >>= Tx.lockToContract spend.asset spend.amount contractHash in
           ET.retT txSkeleton2
           """
    , (Error "illegal creation/destruction of tokens" : Result<Transaction, string>))
    |> shouldEqual
