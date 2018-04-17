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
    | error -> failwith <| sprintf "%A" error

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
let ``Contract activation with too low contract activation sacrifice``() =
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
let ``Contract extension with too low contract extension sacrifice``() =
    let code = SampleContract.sampleContractCode
    let cHash = Contract.computeHash code
    let rootAccount = createTestAccount()

    let tx =
        Account.createActivateContractTransaction localParams code 1ul rootAccount
        |> unwrap

    let _, acs =
        validateInContext 1ul ActiveContractSet.empty utxoSet (Transaction.hash tx) tx
        |> unwrap

    let outpoint = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ExtensionSacrifice cHash;spend={amount=1UL;asset=Constants.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - 1UL}}
        ]

    let tx =
        {contract=None; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]

    let expected:TxResult = General "Contract must be activated for at least one block" |> Error

    validateInContext 1ul acs utxoSet (Transaction.hash tx) tx
    |> should equal expected
    
[<Test>]
[<ParallelizableAttribute>]
let ``Contract extension of a non active contract should fail``() =
    let cHash = Hash.compute "1"B
    let rootAccount = createTestAccount()

    let outpoint = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ExtensionSacrifice cHash;spend={amount=1UL;asset=Constants.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - 1UL}}
        ]

    let tx =
        {contract=None; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]

    let expected:TxResult = General "Contract(s) must be active" |> Error

    validateInContext 1ul ActiveContractSet.empty utxoSet (Transaction.hash tx) tx
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
let ``Contract extension with asset other than zen should fail``() =
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
    let output = {lock=ExtensionSacrifice Hash.zero;spend={amount=1UL;asset=asset}}


    let tx =
        {contract = None; inputs=[Outpoint outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [rootKeyPair]
    let txHash = Transaction.hash tx

    let expected:TxResult = General "Sacrifice must be paid in Zen" |> Error

    validateInContext 1ul ActiveContractSet.empty utxoSet txHash tx
    |> should equal expected
    
[<Test>]
[<ParallelizableAttribute>]
let ``Contract activation with exact amount``() =
    let code = SampleContract.sampleContractCode
    let cHash = Contract.computeHash code
    
    let rootAccount = createTestAccount()
    
    let initialBlock = 10ul
    let blocks = 123ul
    
    let tx =
        Account.createActivateContractTransaction localParams code blocks rootAccount
        |> unwrap

    let _, acs = 
        validateInContext initialBlock ActiveContractSet.empty utxoSet (Transaction.hash tx) tx
        |> unwrap
    
    match ActiveContractSet.tryFind cHash acs with
    | Some contract ->
        contract.expiry |> should equal (blocks + initialBlock)
    | _ -> failwith "contract is not active"

[<Test>]
[<ParallelizableAttribute>]
let ``Contract extension with exact amount``() =
    let code = SampleContract.sampleContractCode
    let cHash = Contract.computeHash code
    
    let rootAccount = createTestAccount()
    
    let initialBlock = 10ul
    let activateBlocks = 123ul
    let extendBlocks = 45ul
    let extensionSacrifice = localParams.sacrificePerByteBlock * (String.length code |> uint64) * (uint64 extendBlocks)
    
    let tx =
        Account.createActivateContractTransaction localParams code activateBlocks rootAccount
        |> unwrap

    let _, acs = 
        validateInContext initialBlock ActiveContractSet.empty utxoSet (Transaction.hash tx) tx
        |> unwrap

    match ActiveContractSet.tryFind cHash acs with
    | Some contract ->
        contract.expiry |> should equal (activateBlocks + initialBlock)
    | _ -> failwith "contract is not active"

    let outpoint = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ExtensionSacrifice cHash;spend={amount=extensionSacrifice;asset=Constants.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - extensionSacrifice}}
        ]

    let tx =
        {contract=None; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]

    let _, acs = 
        validateInContext initialBlock acs utxoSet (Transaction.hash tx) tx
        |> unwrap
    
    match ActiveContractSet.tryFind cHash acs with
    | Some contract ->
        contract.expiry |> should equal (activateBlocks + extendBlocks + initialBlock)
    | _ -> failwith "contract is not active"

[<Test>]
[<ParallelizableAttribute>]
let ``Contract extension with more than one output``() =
    let code = SampleContract.sampleContractCode
    let cHash = Contract.computeHash code
    
    let rootAccount = createTestAccount()
    
    let initialBlock = 10ul
    let activateBlocks = 123ul
    let extendBlocks1 = 45ul
    let extendBlocks2 = 3ul
    let extensionSacrifice1 = localParams.sacrificePerByteBlock * (String.length code |> uint64) * (uint64 extendBlocks1)
    let extensionSacrifice2 = localParams.sacrificePerByteBlock * (String.length code |> uint64) * (uint64 extendBlocks2)
    
    let tx =
        Account.createActivateContractTransaction localParams code activateBlocks rootAccount
        |> unwrap

    let _, acs = 
        validateInContext initialBlock ActiveContractSet.empty utxoSet (Transaction.hash tx) tx
        |> unwrap

    match ActiveContractSet.tryFind cHash acs with
    | Some contract ->
        contract.expiry |> should equal (activateBlocks + initialBlock)
    | _ -> failwith "contract is not active"

    let outpoint = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> fst
    let outputs =
        let output = Account.getUnspentOutputs (rootAccount |> fst) |> fst |> Map.toSeq |> Seq.head |> snd

        [
            {lock=ExtensionSacrifice cHash;spend={amount=extensionSacrifice1;asset=Constants.Zen}}
            {lock=ExtensionSacrifice cHash;spend={amount=extensionSacrifice2;asset=Constants.Zen}}
            {output with spend={output.spend with amount = output.spend.amount - extensionSacrifice1 - extensionSacrifice2}}
        ]

    let tx =
        {contract=None; inputs=[Outpoint outpoint]; outputs=outputs;witnesses=[]}
        |> Transaction.sign [rootKeyPair]

    let _, acs = 
        validateInContext initialBlock acs utxoSet (Transaction.hash tx) tx
        |> unwrap
    
    match ActiveContractSet.tryFind cHash acs with
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
    |> should equal expected
    