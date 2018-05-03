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

let contractPath =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]


[<Literal>]
let rlimit = 2723280u

let getUTXO _ = UtxoSet.NoOutput

let compile code = result {
    let! hints = Contract.recordHints code
    let! queries = Infrastructure.ZFStar.totalQueries hints

    let contract = {
        code = code
        hints = hints
        rlimit = rlimit
        queries = queries
    }

    return!
        Contract.compile contractPath contract
        |> Result.bind (Contract.load contractPath 100ul code)
}

let context = { blockNumber=100u; timestamp = 1_000_000UL}

[<Test>]
let ``Should compile``() =
    compile sampleContractCode
    |> Result.mapError failwith
    |> ignore

[<Test>]
[<ParallelizableAttribute>]
let ``Should get 'elaborate' error for invalid code``() =
    (compile (sampleContractCode + "###")
    , (Error "elaborate" : Result<Contract.T, string>))
    |> shouldEqual

let mapError = function
    | ValidationError.ValidationError.General error -> error
    | other -> other.ToString()

let validateInputs (contract:Contract.T) utxos tx  =
    let acs = ActiveContractSet.add contract.contractId contract ActiveContractSet.empty
    TransactionValidation.validateInContext Chain.localParameters getUTXO contractPath 1ul 1_000_000UL acs Map.empty utxos (Transaction.hash tx) tx
    |> Result.mapError mapError

let validateBasic tx  =
    TransactionValidation.validateBasic tx
    |> Result.mapError mapError

let compileRunAndValidate inputTx utxoSet code =
    compile code
    |> Result.bind (fun contract ->
        Contract.run contract inputTx context "" Anonymous None List.empty
        |> Result.bind (fun (tx, _) ->
            let cost = Contract.getCost contract inputTx context "" Anonymous None List.empty
            tx
            |> TxSkeleton.checkPrefix inputTx
            |> Result.map (fun finalTxSkeleton ->
                let cw = TxSkeleton.getContractWitness contract.contractId "" None inputTx finalTxSkeleton cost
                Transaction.fromTxSkeleton finalTxSkeleton
                |> Transaction.addWitnesses [ ContractWitness cw ])
            |> Result.map (Transaction.sign [ sampleKeyPair ])
            |> Result.bind validateBasic
            |> Result.bind (validateInputs contract utxoSet))
            |> Result.map (fun (tx, _, _) -> tx)
    )
let utxoSet =
    getSampleUtxoset (UtxoSet.asDatabase)

[<Test>]
let ``Contract generated transaction should be valid``() =
    (compileRunAndValidate sampleInputTx utxoSet sampleContractCode
    , (Ok sampleExpectedResult : Result<Transaction, string>))
    |> fun (a,b) -> printfn "%A %A" a b
    //|> shouldEqual

[<Test>]
[<ParallelizableAttribute>]
let ``Should get expected contract cost``() =
    (compile sampleContractCode
     |> Result.map (fun contract ->
        Contract.getCost contract sampleInputTx context "" Anonymous None List.empty)
    , (Ok 212L : Result<int64, string>))
    |> shouldEqual

[<Test>]
[<ParallelizableAttribute>]
let ``Contract should be able to create its own tokens``() =
    compileRunAndValidate sampleInputTx utxoSet
         """
         open Zen.Types
         open Zen.Base
         open Zen.Cost

         module RT = Zen.ResultT
         module Tx = Zen.TxSkeleton

         let main txSkeleton _ contractHash command sender data wallet =
           let! asset = Zen.Asset.getDefault contractHash in
           let lock = ContractLock contractHash in
           let spend = {
               asset=asset;
               amount=1000UL
               } in

           let pInput = Mint spend in

           let! txSkeleton =
               Tx.addInput pInput txSkeleton
               >>= Tx.lockToContract spend.asset spend.amount contractHash in

           RT.ok (txSkeleton, None)

           val cf: txSkeleton -> context -> string -> sender -> option data -> wallet -> cost nat 9
                    let cf _ _ _ _ _ _ = ret (64 + (64 + 64 + 0) + 21)
           """
    |> Result.mapError failwith
    |> ignore

[<Test>]
[<ParallelizableAttribute>]
let ``Contract should not be able to create zero amounts``() =
    (compileRunAndValidate sampleInputTx utxoSet
         """
         open Zen.Types
         open Zen.Base
         open Zen.Cost

         module RT = Zen.ResultT
         module Tx = Zen.TxSkeleton

         let main txSkeleton _ contractHash command sender data wallet =
           let! asset = Zen.Asset.getDefault contractHash in
           let lock = ContractLock contractHash in
           let spend = {
               asset=asset;
               amount=0UL
               } in

           let pInput = Mint spend in

           let! txSkeleton =
               Tx.addInput pInput txSkeleton
               >>= Tx.lockToContract spend.asset spend.amount contractHash in

           RT.ok (txSkeleton, None)

           val cf: txSkeleton -> context -> string -> sender -> option data -> wallet -> cost nat 9
                    let cf _ _ _ _ _ _ = ret (64 + (64 + 64 + 0) + 21)
           """
    , (Error "structurally invalid input(s)" : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
[<ParallelizableAttribute>]
let ``Contract should not be able to create tokens other than its own``() =
    (compileRunAndValidate sampleInputTx utxoSet
         """
         open Zen.Types
         open Zen.Base
         open Zen.Cost

         module RT = Zen.ResultT
         module Tx = Zen.TxSkeleton

         let main txSkeleton _ contractHash command sender data wallet =
           let asset = Zen.Asset.zenAsset in
           let lock = ContractLock contractHash in
           let spend = {
               asset=asset;
               amount=1000UL
               } in

           let pInput = Mint spend in

           let! txSkeleton =
               Tx.addInput pInput txSkeleton
               >>= Tx.lockToContract spend.asset spend.amount contractHash in

           RT.ok (txSkeleton, None)

         let cf _ _ _ _ _ _ = ret (64 + 64 + 18 <: nat)
         """
    , (Error "illegal creation of tokens" : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
[<ParallelizableAttribute>]
let ``Contract should be able to destroy its own tokens locked to it``() =
    let sampleContractCode = """
    open Zen.Types
    open Zen.Base
    open Zen.Cost

    module RT = Zen.ResultT
    module Tx = Zen.TxSkeleton

    let main txSkeleton _ contractHash command sender data wallet =
        let! asset = Zen.Asset.getDefault contractHash in
        let! txSkeleton1 = Tx.destroy 1000UL asset txSkeleton in
        RT.ok (txSkeleton1, None)

    val cf: txSkeleton -> context -> string -> sender -> option data -> wallet -> cost nat 7
        let cf _ _ _ _ _ _ = ret (64 + (64 + 0) + 11)

    """

    let sampleContractId = Contract.makeContractId Version0 sampleContractCode

    let outputToDestroy = {
        lock = PK (PublicKey.hash samplePublicKey)
        spend = { asset = Asset.defaultOf sampleContractId; amount = 1000UL }
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

    let sampleContractTester txSkeleton contractId =
        let output = {
            lock = Destroy
            spend =
            {
                asset = Asset.defaultOf contractId
                amount = 1000UL
            }
        }

        let outputs' = txSkeleton.outputs @ [ output ]
        { txSkeleton with outputs = outputs' }

    let sampleOutputTx =
        sampleContractTester sampleInputTx sampleContractId

    let sampleExpectedResult =
        Transaction.fromTxSkeleton sampleOutputTx
        |> Transaction.addWitnesses [ ContractWitness <| TxSkeleton.getContractWitness sampleContractId "" None sampleInputTx sampleOutputTx 139L]
        |> Transaction.sign [ sampleKeyPair ]

    (compileRunAndValidate sampleInputTx utxoSet sampleContractCode
    , (Ok sampleExpectedResult : Result<Transaction, string>))
    |> shouldEqual