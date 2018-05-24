module Consensus.Tests.ContractActivationTests

open Consensus
open Consensus.Types
open Consensus.ValidationError
open Consensus.TransactionValidation
open Wallet
open NUnit.Framework
open FsUnit
open TestsInfrastructure.Constraints
open Helper

let chain = Chain.Local
let localParams = Chain.getChainParameters chain
let getUTXO _ = UtxoSet.NoOutput
let contractPath =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]

let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx

let validateInContext = validateInContext localParams getUTXO contractPath

type TxResult = Result<Transaction*ActiveContractSet.T*ContractCache.T,ValidationError>

let unwrap =
    function
    | Ok value -> value
    | error -> failwith <| sprintf "%A" error

let recordHints =
    Consensus.Contract.recordHints
    >> unwrap

let totalQueries =
    Infrastructure.ZFStar.totalQueries
    >> unwrap

let (sampleContractId, sampleContractRecord) as sampleContractWithId =
    unwrap SampleContract.contractWithId

[<Test>]
[<ParallelizableAttribute>]
let ``Contract activation without contract sacrifice should fail``() =

    let rootAccount = createTestAccount() |> fst

    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd

    let tx =
        {version = Version0; contract = Some (V0 sampleContractRecord); inputs=[Outpoint outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [rootKeyPair]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Contract activation must include activation sacrifice" |> Error

    validateInContext 1ul 1_000_000UL ActiveContractSet.empty ContractCache.empty utxoSet txHash tx
    |> should equal expected

[<Test>]
[<ParallelizableAttribute>]
let ``Contract activation with too low contract activation sacrifice``() =

    let rootAccount = createTestAccount() |> fst

    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ActivationSacrifice;spend={amount=1UL;asset=Asset.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - 1UL}}
        ]

    let tx =
        {version = Version0; contract = Some (V0 sampleContractRecord); inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Contract must be activated for at least one block" |> Error

    validateInContext 1ul 1_000_000UL ActiveContractSet.empty ContractCache.empty utxoSet txHash tx
    |> should equal expected

[<Test>]
[<ParallelizableAttribute>]
let ``Contract extension with too low contract extension sacrifice``() =
    let rootAccount = createTestAccount()

    let tx =
        Account.createActivationTransactionFromContract localParams sampleContractWithId 1ul rootAccount
        |> unwrap

    let _, acs, _ =
        validateInContext 1ul 1_000_000UL ActiveContractSet.empty ContractCache.empty utxoSet (Transaction.hash tx) tx
        |> unwrap

    let outpoint = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ExtensionSacrifice sampleContractId;spend={amount=1UL;asset=Asset.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - 1UL}}
        ]

    let tx =
        {version = Version0; contract=None; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]

    let expected:TxResult = General "Contract must be activated for at least one block" |> Error

    validateInContext 1ul 1_000_000UL acs ContractCache.empty utxoSet (Transaction.hash tx) tx
    |> should equal expected

[<Test>]
[<ParallelizableAttribute>]
let ``Contract extension of a non active contract should fail``() =
    let contractId = Contract.makeContractId Version0 "1"
    let rootAccount = createTestAccount()

    let outpoint = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ExtensionSacrifice contractId;spend={amount=1UL;asset=Asset.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - 1UL}}
        ]

    let tx =
        {version = Version0; contract=None; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]

    let expected:TxResult = General "Contract(s) must be active" |> Error

    validateInContext 1ul 1_000_000UL ActiveContractSet.empty ContractCache.empty utxoSet (Transaction.hash tx) tx
    |> should equal expected

[<Test>]
[<ParallelizableAttribute>]
let ``Contract activation with asset other than zen should fail``() =

    let asset = Asset (ContractId (Version0, Hash.compute "1"B),Hash.zero)

    let originTx = {
        version = Version0
        inputs=[];
        outputs=[{lock=PK rootPKHash;spend={amount=1UL;asset=asset}}]
        witnesses=[]
        contract=None
    }
    let originTxHash = Transaction.hash originTx

    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO originTxHash originTx

    let outpoint = {txHash=originTxHash;index=0ul}
    let output = {lock=ActivationSacrifice;spend={amount=1UL;asset=asset}}

    let tx =
        {version=Version0; contract = Some (V0 sampleContractRecord); inputs=[Outpoint outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [rootKeyPair]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Sacrifice must be paid in Zen" |> Error

    validateInContext 1ul 1_000_000UL ActiveContractSet.empty ContractCache.empty utxoSet txHash tx
    |> should equal expected

[<Test>]
[<ParallelizableAttribute>]
let ``Contract extension with asset other than zen should fail``() =
    let asset = Asset (ContractId (Version0, Hash.compute "1"B),Hash.zero)

    let originTx = {
        version = Version0
        inputs=[];
        outputs=[{lock=PK rootPKHash;spend={amount=1UL;asset=asset}}]
        witnesses=[]
        contract=None
    }
    let originTxHash = Transaction.hash originTx

    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO originTxHash originTx

    let outpoint = {txHash=originTxHash;index=0ul}
    let output = {lock=ExtensionSacrifice <| ContractId (Version0,Hash.zero);spend={amount=1UL;asset=asset}}


    let tx =
        {version = Version0; contract = None; inputs=[Outpoint outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [rootKeyPair]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Sacrifice must be paid in Zen" |> Error

    validateInContext 1ul 1_000_000UL ActiveContractSet.empty ContractCache.empty utxoSet txHash tx
    |> should equal expected

[<Test>]
[<ParallelizableAttribute>]
let ``Contract activation with exact amount``() =
    let contractId=SampleContract.sampleContractId

    let rootAccount = createTestAccount()

    let initialBlock = 10ul
    let initialTime = 1_000_000UL
    let blocks = 123ul

    let tx =
        Account.createActivationTransactionFromContract localParams sampleContractWithId blocks rootAccount
        |> unwrap

    let _, acs, _ =
        validateInContext initialBlock initialTime ActiveContractSet.empty ContractCache.empty utxoSet (Transaction.hash tx) tx
        |> unwrap

    match ActiveContractSet.tryFind contractId acs with
    | Some contract ->
        contract.expiry |> should equal (blocks + initialBlock)
    | _ -> failwith "contract is not active"

[<Test>]
[<ParallelizableAttribute>]
let ``Contract extension with exact amount``() =
    let code = SampleContract.sampleContractCode
    let contractId = SampleContract.sampleContractId

    let rootAccount = createTestAccount()

    let initialBlock = 10ul
    let initialTime = 1_000_000UL
    let activateBlocks = 123ul
    let extendBlocks = 45ul
    let extensionSacrifice = localParams.sacrificePerByteBlock * (String.length code |> uint64) * (uint64 extendBlocks)

    let tx =
        Account.createActivationTransactionFromContract localParams sampleContractWithId activateBlocks rootAccount
        |> unwrap

    let _, acs, _ =
        validateInContext initialBlock initialTime ActiveContractSet.empty ContractCache.empty utxoSet (Transaction.hash tx) tx
        |> unwrap

    match ActiveContractSet.tryFind contractId acs with
    | Some contract ->
        contract.expiry |> should equal (activateBlocks + initialBlock)
    | _ -> failwith "contract is not active"

    let outpoint = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ExtensionSacrifice contractId;spend={amount=extensionSacrifice;asset=Asset.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - extensionSacrifice}}
        ]

    let tx =
        {version = Version0; contract=None; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]

    let _, acs, _ =
        validateInContext initialBlock initialTime acs ContractCache.empty utxoSet (Transaction.hash tx) tx
        |> unwrap

    match ActiveContractSet.tryFind contractId acs with
    | Some contract ->
        contract.expiry |> should equal (activateBlocks + extendBlocks + initialBlock)
    | _ -> failwith "contract is not active"

[<Test>]
[<ParallelizableAttribute>]
let ``Contract extension with more than one output``() =
    let code = SampleContract.sampleContractCode
    let contractId = SampleContract.sampleContractId

    let rootAccount = createTestAccount()

    let initialBlock = 10ul
    let initialTime = 1_000_000UL
    let activateBlocks = 123ul
    let extendBlocks1 = 45ul
    let extendBlocks2 = 3ul
    let extensionSacrifice1 = localParams.sacrificePerByteBlock * (String.length code |> uint64) * (uint64 extendBlocks1)
    let extensionSacrifice2 = localParams.sacrificePerByteBlock * (String.length code |> uint64) * (uint64 extendBlocks2)

    let tx =
        Account.createActivationTransactionFromContract localParams sampleContractWithId activateBlocks rootAccount
        |> unwrap

    let _, acs, _ =
        validateInContext initialBlock initialTime ActiveContractSet.empty ContractCache.empty utxoSet (Transaction.hash tx) tx
        |> unwrap

    match ActiveContractSet.tryFind contractId acs with
    | Some contract ->
        contract.expiry |> should equal (activateBlocks + initialBlock)
    | _ -> failwith "contract is not active"

    let outpoint = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ExtensionSacrifice contractId;spend={amount=extensionSacrifice1;asset=Asset.Zen}}
            {lock=ExtensionSacrifice contractId;spend={amount=extensionSacrifice2;asset=Asset.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - extensionSacrifice1 - extensionSacrifice2}}
        ]

    let tx =
        {version = Version0; contract=None; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]

    let _, acs, _ =
        validateInContext initialBlock initialTime acs ContractCache.empty utxoSet (Transaction.hash tx) tx
        |> unwrap

    match ActiveContractSet.tryFind contractId acs with
    | Some contract ->
        contract.expiry |> should equal (activateBlocks + extendBlocks1 + extendBlocks2 + initialBlock)
    | _ -> failwith "contract is not active"

[<Test>]
let ``Contract activation without hints should fail``() =
    let code = SampleContract.sampleContractCode

    let rootAccount = createTestAccount() |> fst

    let activationSacrificeAmount = 1000UL
    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ActivationSacrifice;spend={amount=activationSacrificeAmount;asset=Asset.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - activationSacrificeAmount}}
        ]

    let tx =
        {version = Version0; contract = Some (V0 { code=code;hints="";rlimit=0u;queries=0u }); inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]


    let expected:TxResult = General "total queries: invalid hints" |> Error

    validateInContext 1ul 1_000_000UL ActiveContractSet.empty ContractCache.empty utxoSet (Transaction.hash tx) tx
    |> should equal expected

[<Test>]
let ``Contract activation with invalid queries should fail``() =

    let rootAccount = createTestAccount() |> fst

    let activationSacrificeAmount = 1000UL
    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ActivationSacrifice;spend={amount=activationSacrificeAmount;asset=Asset.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - activationSacrificeAmount}}
        ]

    let {queries=totalQueries} = sampleContractRecord

    let tx =
        {version = Version0; contract = Some (V0 { sampleContractRecord with queries=(totalQueries - 1u)}); inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]


    let expected:TxResult = General "Total queries mismatch" |> Error

    validateInContext 1ul 1_000_000UL ActiveContractSet.empty ContractCache.empty utxoSet (Transaction.hash tx) tx
    |> should equal expected

[<Test>]
let ``Contract with activation sacrifice but without a contract should fail``() =
    let rootAccount = createTestAccount() |> fst

    let activationSacrificeAmount = 1000UL
    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd
        [
            {lock=ActivationSacrifice;spend={amount=activationSacrificeAmount;asset=Asset.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - activationSacrificeAmount}}
        ]

    let tx =
        {version = Version0; contract = None; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]


    let expected:Result<Transaction,ValidationError> = General "tx with an activation sacrifice must include a contract" |> Error

    validateBasic tx
    |> should equal expected