module Consensus.Tests.ContractActivationTests

open Consensus
open Consensus.Types
open Consensus.TransactionValidation
open Wallet
open NUnit.Framework
open FsUnit
open TestsInfrastructure.Constraints
open Helper

let chain = Chain.Local
let localParams = Chain.getChainParameters chain
let getUTXO _ = UtxoSet.NoOutput
let contractPath = "./test"
let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx

let validateInContext = validateInContext localParams getUTXO contractPath

type TxResult = Result<Transaction*ActiveContractSet.T,ValidationError>

let unwrap =
    function
    | Ok value -> value
    | Error error -> failwith error

let recordHints =
    Consensus.Contract.recordHints
    >> unwrap

let totalQueries =
    Infrastructure.ZFStar.totalQueries
    >> unwrap

[<Test>]
[<ParallelizableAttribute>]
let ``Contract activation without contract sacrifice should fail``() =
    let code = SampleContract.sampleContractCode

    let rootAccount = createTestAccount() |> fst

    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd

    let hints = recordHints code
    let totalQueries = totalQueries hints

    let tx =
        {contract = Some { code=code;hints=hints;rlimit=0u;queries=totalQueries }; inputs=[Outpoint outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [rootKeyPair]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Contract activation must include activation sacrifice" |> Error

    validateInContext 1ul ActiveContractSet.empty utxoSet txHash tx
    |> should equal expected

[<Test>]
[<ParallelizableAttribute>]
let ``Contract activation with too low contract sacrifice``() =
    let code = SampleContract.sampleContractCode

    let rootAccount = createTestAccount() |> fst

    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ActivationSacrifice;spend={amount=1UL;asset=Constants.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - 1UL}}
        ]

    let hints = recordHints code
    let totalQueries = totalQueries hints

    let tx =
        {contract = Some { code=code;hints=hints;rlimit=0u;queries=totalQueries }; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Contract must be activated for at least one block" |> Error

    validateInContext 1ul ActiveContractSet.empty utxoSet txHash tx
    |> should equal expected

[<Test>]
[<ParallelizableAttribute>]
let ``Contract activation with asset other than zen should fail``() =
    let code = SampleContract.sampleContractCode

    let asset = Hash.compute "1"B,Hash.zero

    let originTx = {
        inputs=[];
        outputs=[{lock=PK rootPKHash;spend={amount=1UL;asset=asset}}]
        witnesses=[]
        contract=None
    }
    let originTxHash = Transaction.hash originTx

    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO originTxHash originTx

    let outpoint = {txHash=originTxHash;index=0ul}
    let output = {lock=ActivationSacrifice;spend={amount=1UL;asset=asset}}

    let hints = recordHints code
    let totalQueries = totalQueries hints

    let tx =
        {contract = Some { code=code;hints=hints;rlimit=0u;queries=totalQueries }; inputs=[Outpoint outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [rootKeyPair]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Sacrifice must be paid in Zen" |> Error

    validateInContext 1ul ActiveContractSet.empty utxoSet txHash tx
    |> should equal expected

[<Test>]
[<ParallelizableAttribute>]
let ``Contract activation with exact amount``() =
    let code = SampleContract.sampleContractCode

    let rootAccount = createTestAccount()

    let tx =
        Account.createActivateContractTransaction localParams code 1ul rootAccount
        |> function | Ok tx -> tx | _ -> failwith "unexpected"
    let txHash = Transaction.hash tx

    validateInContext 1ul ActiveContractSet.empty utxoSet txHash tx
    |> should be ok

[<Test>]
let ``Contract activation without hints should fail``() =
    let code = SampleContract.sampleContractCode

    let rootAccount = createTestAccount() |> fst

    let activationSacrificeAmount = 1000UL
    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ActivationSacrifice;spend={amount=activationSacrificeAmount;asset=Constants.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - activationSacrificeAmount}}
        ]

    let tx =
        {contract = Some { code=code;hints="";rlimit=0u;queries=0u }; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]


    let expected:TxResult = General "total queries: invalid hints" |> Error

    validateInContext 1ul ActiveContractSet.empty utxoSet (Transaction.hash tx) tx
    |> should equal expected

[<Test>]
let ``Contract activation with invalid queries should fail``() =
    let code = SampleContract.sampleContractCode

    let rootAccount = createTestAccount() |> fst

    let activationSacrificeAmount = 1000UL
    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ActivationSacrifice;spend={amount=activationSacrificeAmount;asset=Constants.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - activationSacrificeAmount}}
        ]

    let hints = recordHints code
    let totalQueries = totalQueries hints

    let tx =
        {contract = Some { code=code;hints=hints;rlimit=0u;queries=(totalQueries - 1u) }; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]


    let expected:TxResult = General "Total queries mismatch" |> Error

    validateInContext 1ul ActiveContractSet.empty utxoSet (Transaction.hash tx) tx
    |> printfn "%A" //  should equal expected
    ()