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
open Consensus.UtxoSet
open TestsInfrastructure.Nunit

let result = new Infrastructure.Result.ResultBuilder<string>()

let contractPath = "./test"

let getUTXO _ = UtxoSet.NoOutput

let compile code = result {
    let! hints = Contract.recordHints code
    return! Contract.compile contractPath (code, hints)
}

let compileAndCheck code =
    compile code
    |> Result.map (fun contract ->
        // check hash validity
        (Hash.isValid contract.hash, true)
        |> shouldEqual
        (System.Text.Encoding.UTF8.GetBytes(code:string) |> Hash.compute, contract.hash)
        |> shouldEqual
        contract)

[<Test>]
let ``Should compile``() =
    compileAndCheck sampleContractCode
    |> Result.mapError failwith
    |> ignore

[<Test>]
let ``Should get 'elaborate' error for invalid code``() =
    (compileAndCheck (sampleContractCode + "###")
    , (Error "elaborate" : Result<Contract.T, string>))
    |> shouldEqual

let validateInputs (contract:Contract.T) utxos tx  =
    let acs = ActiveContractSet.add contract.hash contract ActiveContractSet.empty
    TransactionValidation.validateInContext getUTXO contractPath acs utxos (Transaction.hash tx) tx
    |> Result.mapError (function
        | TransactionValidation.ValidationError.General error -> error
        | other -> other.ToString())

let compileRunAndValidate inputTx utxoSet code =
    compileAndCheck code
    |> Result.bind (fun contract ->
        Contract.run contract "" None List.empty inputTx
        |> Result.bind (TxSkeleton.checkPrefix inputTx)
        |> Result.map (fun finalTxSkeleton ->
            let tx = Transaction.fromTxSkeleton finalTxSkeleton
            Transaction.addContractWitness contract.hash "" (PK Hash.zero) inputTx finalTxSkeleton tx)
        |> Result.map (Transaction.sign [ sampleKeyPair ])
        |> Result.bind (validateInputs contract utxoSet))
        |> Result.map fst

let utxoSet = 
    getSampleUtxoset (UtxoSet.asDatabase)

[<Test>]
let ``Contract generated transaction should be valid``() =
    (compileRunAndValidate sampleInputTx utxoSet sampleContractCode
    , (Ok sampleExpectedResult : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
let ``Should get expected contract cost``() =
    (compile sampleContractCode
     |> Result.bind (fun contract ->
        Contract.getCost contract "" None List.empty sampleInputTx)
    , (Ok 148I : Result<bigint, string>))
    |> shouldEqual

[<Test>]
let ``Contract should not be able to create tokens other than its own``() =
    (compileRunAndValidate sampleInputTx utxoSet
         """
         open Zen.Types
         open Zen.Vector
         open Zen.Util
         open Zen.Base
         open Zen.Cost

         module ET = Zen.ErrorT
         module Tx = Zen.TxSkeleton

         val cf: txSkeleton -> string -> option lock -> #l:nat -> wallet l -> cost nat 1
         let cf _ _ _ #l _ = ret 149

         val main: txSkeleton -> hash -> string -> option lock -> #l:nat -> wallet l -> cost (result txSkeleton) 149
         let main txSkeleton contractHash command returnAddress #l wallet =
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
    , (Error "illegal creation of tokens" : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
let ``Contract should be able to destroy its own tokens locked to it``() =
    let sampleContractCode = """
    open Zen.Types
    open Zen.Vector
    open Zen.Util
    open Zen.Base
    open Zen.Cost

    module ET = Zen.ErrorT
    module Tx = Zen.TxSkeleton

    val cf: txSkeleton -> string -> option lock -> #l:nat -> wallet l -> cost nat 3
    let cf _ _ _ #l _ = ret (64 + 4)

    val main: txSkeleton -> hash -> string -> option lock -> #l:nat -> wallet l -> cost (result txSkeleton) (64 + 4)
    let main txSkeleton contractHash command returnAddress #l wallet =
      let txSkeleton1 = Tx.destroy 1000UL contractHash txSkeleton in
      ET.retT txSkeleton1"""

    let sampleContractHash =
        sampleContractCode
        |> Encoding.UTF8.GetBytes
        |> Hash.compute

    let outputToDestroy = {
        lock = Contract sampleContractHash
        spend = { asset = sampleContractHash; amount = 1000UL }
    }
    
    let sampleInput2 = {
        txHash = Hash.zero
        index = 2u
    }

    let sampleInputTx =
        {
            pInputs = [ (sampleInput, sampleOutput) ; (sampleInput2, outputToDestroy) ]
            outputs = [ sampleOutput ]
        }
        
    let utxoSet = 
        getSampleUtxoset (UtxoSet.asDatabase)
        |> Map.add sampleInput2 (OutputStatus.Unspent outputToDestroy)

    let sampleContractTester txSkeleton cHash =
        let output = {
            lock = Destroy
            spend =
            {
                asset = cHash
                amount = 1000UL
            }
        }
    
        let outputs' = txSkeleton.outputs @ [ output ]
        { txSkeleton with outputs = outputs' }

    let sampleOutputTx =
        sampleContractTester sampleInputTx sampleContractHash
    
    let sampleExpectedResult =
        let tx = Transaction.fromTxSkeleton sampleOutputTx    
        Transaction.addContractWitness sampleContractHash "" (PK Hash.zero) sampleInputTx sampleOutputTx tx
        |> Transaction.sign [ sampleKeyPair ]
        
    (compileRunAndValidate sampleInputTx utxoSet sampleContractCode
    , (Ok sampleExpectedResult : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
let ``Contract should not be able to destroy tokens other than its own - single output``() =
    let contractCode = """
    open Zen.Types
    open Zen.Vector
    open Zen.Util
    open Zen.Base
    open Zen.Cost
    open Zen.Assets

    module ET = Zen.ErrorT
    module Tx = Zen.TxSkeleton

    val cf: txSkeleton -> string -> option lock -> #l:nat -> wallet l -> cost nat 3
    let cf _ _ _ #l _ = ret (64 + 4)

    val main: txSkeleton -> hash -> string -> option lock -> #l:nat -> wallet l -> cost (result txSkeleton) (64 + 4)
    let main txSkeleton contractHash command returnAddress #l wallet =
      let txSkeleton1 = Tx.destroy 1000UL zenAsset txSkeleton in // should be impossible
      ET.retT txSkeleton1"""
      
    let contractHash =
        (contractCode : string)
        |> Encoding.UTF8.GetBytes
        |> Hash.compute

    let outputToDestroy = {
        lock = Contract contractHash
        spend = { asset = contractHash; amount = 1000UL }
    }
    
    let sampleInput2 = {
        txHash = Hash.zero
        index = 2u
    }

    let sampleInputTx =
        {
            pInputs = [ (sampleInput, sampleOutput) ; (sampleInput2, outputToDestroy) ]
            outputs = [ sampleOutput ]
        }
        
    let utxoSet = 
        getSampleUtxoset (UtxoSet.asDatabase)
        |> Map.add sampleInput2 (OutputStatus.Unspent outputToDestroy)

    (compileRunAndValidate sampleInputTx utxoSet contractCode
    , (Error "illegal destruction of tokens" : Result<Transaction, string>))
    |> shouldEqual
    
[<Test>]
let ``Contract should not be able to destroy tokens other than its own - multiple (two) outputs``() =
    let contractCode = """
    open Zen.Types
    open Zen.Vector
    open Zen.Util
    open Zen.Base
    open Zen.Cost
    open Zen.Assets

    module ET = Zen.ErrorT
    module Tx = Zen.TxSkeleton

    val cf: txSkeleton -> string -> option lock -> #l:nat -> wallet l -> cost nat 5
    let cf _ _ _ #l _ = ret (64 + 64 + 8)

    val main: txSkeleton -> hash -> string -> option lock -> #l:nat -> wallet l -> cost (result txSkeleton) (64 + 64 + 8)
    let main txSkeleton contractHash command returnAddress #l wallet =
      let txSkeleton1 = Tx.destroy 1000UL zenAsset txSkeleton in // should be impossible
      let txSkeleton2 = txSkeleton1 >>= Tx.destroy 1000UL contractHash in // should be possible
      ET.retT txSkeleton2"""
      
    let contractHash =
        (contractCode : string)
        |> Encoding.UTF8.GetBytes
        |> Hash.compute

    let outputToDestroy = {
        lock = Contract contractHash
        spend = { asset = contractHash; amount = 1000UL }
    }
    
    let sampleInput2 = {
        txHash = Hash.zero
        index = 2u
    }

    let sampleInputTx =
        {
            pInputs = [ (sampleInput, sampleOutput) ; (sampleInput2, outputToDestroy) ]
            outputs = [ sampleOutput ]
        }
        
    let utxoSet = 
        getSampleUtxoset (UtxoSet.asDatabase)
        |> Map.add sampleInput2 (OutputStatus.Unspent outputToDestroy)

    (compileRunAndValidate sampleInputTx utxoSet contractCode
    , (Error "illegal destruction of tokens" : Result<Transaction, string>))
    |> shouldEqual

