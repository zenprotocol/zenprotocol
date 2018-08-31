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
let getContractState _ = None

let compile code = lazy (result {
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
})

let context = { blockNumber=100u; timestamp = 1_000_000UL}

let compiledSampleContract = compile sampleContractCode

[<Test>]
let ``Should compile``() =
    (compiledSampleContract.Force ())
    |> Result.mapError failwith
    |> ignore

[<Test>]
[<Parallelizable>]
let ``Should get 'elaborate' error for invalid code``() =
    ((compile (sampleContractCode + "###")).Force ()
    , (Error "elaborate" : Result<Contract.T, string>))
    |> shouldEqual

let mapError = function
    | ValidationError.ValidationError.General error -> error
    | other -> other.ToString()

let validateInputs (contract:Contract.T) utxos tx  =
    let acs = ActiveContractSet.add contract.contractId contract ActiveContractSet.empty
    TransactionValidation.validateInContext Chain.localParameters getUTXO contractPath 1ul 1_000_000UL acs Map.empty utxos getContractState ContractStates.asDatabase tx
    |> Result.mapError mapError

let validateBasic tx  =
    TransactionValidation.validateBasic tx
    |> Result.mapError mapError

let runAndValidate inputTx utxoSet (lazyCompiled : Lazy<_>) =
    lazyCompiled.Force ()
    |> Result.bind (fun contract ->
        Contract.run contract inputTx context "" Anonymous None List.empty None
        |> Result.bind (fun (tx, _, _) ->
            Contract.getCost contract inputTx context "" Anonymous None List.empty None
            |> Result.bind (fun cost ->
                tx
                |> TxSkeleton.checkPrefix inputTx
                |> Result.map (fun finalTxSkeleton ->
                    let cw = TxSkeleton.getContractWitness contract.contractId "" None NotCommitted inputTx finalTxSkeleton cost
                    Transaction.fromTxSkeleton finalTxSkeleton
                    |> Transaction.pushWitnesses [ ContractWitness cw ])
                |> Result.map (Transaction.sign [ sampleKeyPair ] TxHash)
                |> Result.bind validateBasic
                |> Result.map Transaction.toExtended
                |> Result.bind (validateInputs contract utxoSet))
                |> Result.map (fun (ex, _, _, _) -> ex.tx))
    )

let compileRunAndValidate inputTx utxoSet code =
    let lazyCompiled = compile code
    runAndValidate inputTx utxoSet lazyCompiled

let utxoSet =
    getSampleUtxoset (UtxoSet.asDatabase)

[<Test>]
let ``Contract generated transaction should be valid``() =
    (runAndValidate sampleInputTx utxoSet compiledSampleContract
    , (Ok sampleExpectedResult : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
[<Parallelizable>]
let ``Should get expected contract cost``() =
    (compiledSampleContract.Force ()
     |> Result.bind (fun contract ->
        Contract.getCost contract sampleInputTx context "" Anonymous None List.empty None)
    , (Ok 214L : Result<int64, string>))
    |> shouldEqual

[<Test>]
[<Parallelizable>]
let ``Contract should be able to create its own tokens``() =
    compileRunAndValidate sampleInputTx utxoSet
        """
        open Zen.Types
        open Zen.Base
        open Zen.Cost

        module RT = Zen.ResultT
        module Tx = Zen.TxSkeleton
        module C = Zen.Cost

        let main txSkeleton _ contractId command sender messageBody wallet state =
            let! asset = Zen.Asset.getDefault contractId in
            let lock = ContractLock contractId in
            let spend = {
                asset=asset;
                amount=1000UL
            } in

            let pInput = Mint spend in

            let! txSkeleton =
                Tx.addInput pInput txSkeleton
                >>= Tx.lockToContract spend.asset spend.amount contractId in

            RT.ok @ { tx = txSkeleton; message = None; state = NoChange}

            let cf _ _ _ _ _ _ _ =
                64 + (64 + 64 + 0) + 23
                |> cast nat
                |> C.ret
           """
    |> Result.mapError failwith
    |> ignore

[<Test>]
[<Parallelizable>]
let ``Contract should not be able to create zero amounts``() =
    (compileRunAndValidate sampleInputTx utxoSet
        """
        open Zen.Types
        open Zen.Base
        open Zen.Cost

        module RT = Zen.ResultT
        module Tx = Zen.TxSkeleton
        module C = Zen.Cost

        let main txSkeleton _ contractId command sender messageBody wallet state =
            let! asset = Zen.Asset.getDefault contractId in
            let lock = ContractLock contractId in
            let spend = {
                asset=asset;
                amount=0UL
            } in

            let pInput = Mint spend in

            let! txSkeleton =
                Tx.addInput pInput txSkeleton
                >>= Tx.lockToContract spend.asset spend.amount contractId in

            RT.ok @ { tx = txSkeleton; message = None; state = NoChange}

            let cf _ _ _ _ _ _ _ =
                64 + (64 + 64 + 0) + 23
                |> cast nat
                |> C.ret
           """
    , (Error "structurally invalid input(s)" : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
[<Parallelizable>]
let ``Contract should not be able to create tokens other than its own``() =
    (compileRunAndValidate sampleInputTx utxoSet
        """
        open Zen.Types
        open Zen.Base
        open Zen.Cost

        module RT = Zen.ResultT
        module Tx = Zen.TxSkeleton
        module C = Zen.Cost

        let main txSkeleton _ contractId command sender messageBody wallet state =
            let asset = Zen.Asset.zenAsset in
            let lock = ContractLock contractId in
            let spend = {
                asset=asset;
                amount=1000UL
            } in

            let pInput = Mint spend in

            let! txSkeleton =
                Tx.addInput pInput txSkeleton
                >>= Tx.lockToContract spend.asset spend.amount contractId in

            RT.ok @ { tx = txSkeleton; message = None; state = NoChange}

        let cf _ _ _ _ _ _ _ =
            64 + 64 + 20
            |> cast nat
            |> C.ret
        """
    , (Error "illegal creation of tokens" : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
[<Parallelizable>]
let ``Contract should be able to destroy its own tokens locked to it``() =
    let contract = """
    open Zen.Types
    open Zen.Base
    open Zen.Cost

    module RT = Zen.ResultT
    module Tx = Zen.TxSkeleton
    module C = Zen.Cost

    let main txSkeleton _ contractId command sender messageBody wallet state =
        let! asset = Zen.Asset.getDefault contractId in
        let! txSkeleton = Tx.destroy 1000UL asset txSkeleton in
        RT.ok @ { tx = txSkeleton; message = None; state = NoChange}

    let cf _ _ _ _ _ _ _ =
        64 + (64 + 0) + 13
        |> cast nat
        |> C.ret
    """

    let contractId = Contract.makeContractId Version0 contract

    let outputToDestroy = {
        lock = PK (PublicKey.hash samplePublicKey)
        spend = { asset = Asset.defaultOf contractId; amount = 1000UL }
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
        sampleContractTester sampleInputTx contractId

    let sampleExpectedResult =
        Transaction.fromTxSkeleton sampleOutputTx
        |> Transaction.pushWitnesses [ ContractWitness <| TxSkeleton.getContractWitness contractId "" None NotCommitted sampleInputTx sampleOutputTx 141L]
        |> Transaction.sign [ sampleKeyPair ] TxHash

    (compileRunAndValidate sampleInputTx utxoSet contract
    , (Ok sampleExpectedResult : Result<Transaction, string>))
    |> shouldEqual

[<Test>]
[<Parallelizable>]
let ``Contract should be able to use its context``() =
    let code = """
    open Zen.Types
    open Zen.Base
    open Zen.Cost

    module RT = Zen.ResultT
    module Tx = Zen.TxSkeleton
    module C = Zen.Cost

    let main txSkeleton contractContext contractId command sender messageBody wallet state =
    let! asset = Zen.Asset.getDefault contractId in
    let lock = ContractLock contractId in
    let spend = {
        asset=asset;
        amount=contractContext.timestamp
    } in

    let pInput = Mint spend in

    let! txSkeleton =
    Tx.addInput pInput txSkeleton
    >>= Tx.lockToContract spend.asset spend.amount contractId in

    RT.ok @ { tx = txSkeleton; message = None; state = NoChange}

    let cf _ _ _ _ _ _ _ =
        64 + (64 + 64 + 0) + 24
        |> cast nat
        |> C.ret
    """
    compileRunAndValidate sampleInputTx utxoSet code
    |> Result.mapError failwith
    |> Result.map (fun tx -> tx.outputs.[1].spend.amount )
    |> Result.map (fun amt ->
        (amt, context.timestamp) |> shouldEqual
    )
    |> ignore
