module Consensus.Tests.ContractTests

open Consensus
open Consensus.Chain
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
    return! Contract.compile contractPath (code, hints) 1000ul
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
    TransactionValidation.validateInContext Chain.Local getUTXO contractPath 1ul acs utxos (Transaction.hash tx) tx
    |> Result.mapError (function
        | TransactionValidation.ValidationError.General error -> error
        | other -> other.ToString())

let compileRunAndValidate inputTx utxoSet code =
    compileAndCheck code
    |> Result.bind (fun contract ->
        Contract.run contract "" Contract.EmptyData None List.empty inputTx
        |> Result.bind (fun (tx, _) ->
            let cost = Contract.getCost contract "" Contract.EmptyData None List.empty inputTx
            tx
            |> TxSkeleton.checkPrefix inputTx
            |> Result.map (fun finalTxSkeleton ->
                let cw = TxSkeleton.getContractWitness contract.hash "" Contract.EmptyData (PK Hash.zero) inputTx finalTxSkeleton cost
                Transaction.fromTxSkeleton finalTxSkeleton
                |> Transaction.addWitnesses [ cw ])
            |> Result.map (Transaction.sign [ sampleKeyPair ])
            |> Result.bind (validateInputs contract utxoSet))
            |> Result.map (fun (tx, _) -> tx)
    )
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
     |> Result.map (fun contract ->
        Contract.getCost contract "" Contract.EmptyData None List.empty sampleInputTx)
    , (Ok 213I : Result<bigint, string>))
    |> shouldEqual

[<Test>]
let ``Contract should not be able to create tokens other than its own``() =
    (compileRunAndValidate sampleInputTx utxoSet
         """
         open Zen.Types
         open Zen.Vector
         open Zen.Base
         open Zen.Cost

         module ET = Zen.ErrorT
         module Tx = Zen.TxSkeleton

         val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 9
         let cf _ _ _ _ #l _ = ret (64 + (64 + 64 + 0) + 19)

         val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message)) (64 + (64 + 64 + 0) + 19)
         let main txSkeleton contractHash command data returnAddress #l wallet =
           let! asset = Zen.Asset.getDefault Zen.Asset.zeroHash in
           let lock = ContractLock contractHash in
           let spend = {
               asset=asset;
               amount=1000UL
               } in

           let pInput = Mint spend in

           let! txSkeleton =
               Tx.addInput pInput txSkeleton
               >>= Tx.lockToContract spend.asset spend.amount contractHash in

           ET.ret (txSkeleton, None)
           """
    , (Error "illegal creation of tokens" : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
let ``Contract should be able to destroy its own tokens locked to it``() =
    let sampleContractCode = """
    open Zen.Types
    open Zen.Vector
    open Zen.Base
    open Zen.Cost

    module ET = Zen.ErrorT
    module Tx = Zen.TxSkeleton

    val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 7
    let cf _ _ _ _ #l _ = ret (64 + (64 + 0) + 9)

    val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message)) (64 + (64 + 0) + 9)
    let main txSkeleton contractHash command data returnAddress #l wallet =
        let! asset = Zen.Asset.getDefault contractHash in
        let! txSkeleton1 = Tx.destroy 1000UL asset txSkeleton in
        ET.ret (txSkeleton1, None)
    """

    let sampleContractHash =
        sampleContractCode
        |> Encoding.UTF8.GetBytes
        |> Hash.compute

    let outputToDestroy = {
        lock = PK (PublicKey.hash samplePublicKey)
        spend = { asset = sampleContractHash, Hash.zero; amount = 1000UL }
    }

    let sampleInput = {
        txHash = Hash.zero
        index = 2u
    }

    let sampleInputTx =
        {
            pInputs = [ PointedOutput (sampleInput, outputToDestroy) ]
            outputs = [ ]
        }

    let utxoSet =
        getSampleUtxoset (UtxoSet.asDatabase)
        |> Map.add sampleInput (OutputStatus.Unspent outputToDestroy)

    let sampleContractTester txSkeleton cHash =
        let output = {
            lock = Destroy
            spend =
            {
                asset = cHash, Hash.zero
                amount = 1000UL
            }
        }

        let outputs' = txSkeleton.outputs @ [ output ]
        { txSkeleton with outputs = outputs' }

    let sampleOutputTx =
        sampleContractTester sampleInputTx sampleContractHash

    let sampleExpectedResult =
        Transaction.fromTxSkeleton sampleOutputTx
        |> Transaction.addWitnesses [ TxSkeleton.getContractWitness sampleContractHash "" Contract.EmptyData (PK Hash.zero) sampleInputTx sampleOutputTx 137I]
        |> Transaction.sign [ sampleKeyPair ]

    (compileRunAndValidate sampleInputTx utxoSet sampleContractCode
    , (Ok sampleExpectedResult : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
let ``Contract should not be able to destroy tokens other than its own - single output``() =
    let contractCode = """
    open Zen.Types
    open Zen.Vector
    open Zen.Base
    open Zen.Cost
    open Zen.Asset

    module ET = Zen.ErrorT
    module Tx = Zen.TxSkeleton

    val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 3
    let cf _ _ _ _ #l _ = ret (64 + 7)

    val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message)) (64 + 7)
    let main txSkeleton contractHash command data returnAddress #l wallet =
        let! txSkeleton1 = Tx.destroy 1000UL zenAsset txSkeleton in // should be impossible
        ET.ret (txSkeleton1, None)
    """

    let contractHash =
        (contractCode : string)
        |> Encoding.UTF8.GetBytes
        |> Hash.compute

    let outputToDestroy = {
        lock = Contract contractHash
        spend = { asset = contractHash, Hash.zero; amount = 1000UL }
    }

    let sampleInput2 = {
        txHash = Hash.zero
        index = 2u
    }

    let sampleInputTx =
        {
            pInputs = [ PointedOutput (sampleInput, sampleOutput) ; PointedOutput (sampleInput2, outputToDestroy) ]
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
    open Zen.Base
    open Zen.Cost
    open Zen.Asset

    module ET = Zen.ErrorT
    module Tx = Zen.TxSkeleton

    val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 9
    let cf _ _ _ _ #l _ = ret (64 + (64 + 64 + 0) + 13)

    val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message)) (64 + (64 + 64 + 0) + 13)
    let main txSkeleton contractHash command data returnAddress #l wallet =
        let! asset = Zen.Asset.getDefault contractHash in
        let txSkeleton1 = Tx.destroy 1000UL zenAsset txSkeleton in // should be impossible
        let! txSkeleton2 = txSkeleton1 >>= Tx.destroy 1000UL asset in // should be possible
        ET.ret (txSkeleton2, None)
    """

    let contractHash =
        (contractCode : string)
        |> Encoding.UTF8.GetBytes
        |> Hash.compute

    let outputToDestroy = {
        lock = Contract contractHash
        spend = { asset = contractHash, Hash.zero; amount = 1000UL }
    }

    let sampleInput2 = {
        txHash = Hash.zero
        index = 2u
    }

    let sampleInputTx =
        {
            pInputs = [ PointedOutput (sampleInput, sampleOutput) ; PointedOutput (sampleInput2, outputToDestroy) ]
            outputs = [ sampleOutput ]
        }

    let utxoSet =
        getSampleUtxoset (UtxoSet.asDatabase)
        |> Map.add sampleInput2 (OutputStatus.Unspent outputToDestroy)

    (compileRunAndValidate sampleInputTx utxoSet contractCode
    , (Error "illegal destruction of tokens" : Result<Transaction, string>))
    |> shouldEqual
