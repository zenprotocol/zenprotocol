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
let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx

let validateInContext = validateInContext localParams getUTXO contractPath

type TxResult = Result<Transaction*ActiveContractSet.T,ValidationError>

[<Test>]
let ``Contract activation without contract sacrifice should fail``() =
    let code = SampleContract.sampleContractCode

    let rootAccount = createTestAccount() |> fst

    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd

    let tx =
        {contract = Some (code,""); inputs=[Outpoint outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [keyPair rootAccount]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Contract activation must include activation sacrifice" |> Error

    validateInContext 1ul ActiveContractSet.empty utxoSet txHash tx
    |> should equal expected

[<Test>]
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

    let tx =
        {contract = Some (code,""); inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [keyPair rootAccount]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Contract must be activated for at least one block" |> Error

    validateInContext 1ul ActiveContractSet.empty utxoSet txHash tx
    |> should equal expected

[<Test>]
let ``Contract activation with asset other than zen should fail``() =
    let code = SampleContract.sampleContractCode

    let asset = Hash.compute "1"B,Hash.zero

    let originTx = {
        inputs=[];
        outputs=[{lock=PK Transaction.rootPKHash;spend={amount=1UL;asset=asset}}]
        witnesses=[]
        contract=None
    }
    let originTxHash = Transaction.hash originTx

    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO originTxHash originTx

    let outpoint = {txHash=originTxHash;index=0ul}
    let output = {lock=ActivationSacrifice;spend={amount=1UL;asset=asset}}

    let tx =
        {contract = Some (code,""); inputs=[Outpoint outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [keyPair (fst rootAccountData)]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Sacrifice must be paid in Zen" |> Error

    validateInContext 1ul ActiveContractSet.empty utxoSet txHash tx
    |> should equal expected

[<Test>]
let ``Contract activation with exact amount``() =
    let code = SampleContract.sampleContractCode

    let rootAccount = createTestAccount()

    let tx =
        Account.createActivateContractTransaction localParams code 1ul rootAccount
        |> function | Ok tx -> tx | _ -> failwith "unexpected"
    let txHash = Transaction.hash tx

    validateInContext 1ul ActiveContractSet.empty utxoSet txHash tx
    |> should be ok


