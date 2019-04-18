module Consensus.Tests.BlockTests

open Consensus
open Consensus.Chain
open Consensus.Types
open Consensus.Tests
open Wallet
open NUnit.Framework
open Infrastructure
open FsCheck
open FsCheck.NUnit
open FsUnit
open TestsInfrastructure.Constraints
open TestsInfrastructure.Nunit
open Consensus.Tests.SampleContract
open Helper
open Chain

let timestamp = 1515594186383UL + 1UL
let difficulty = 0x20fffffful

let chain = getChainParameters Chain.Local

let contractsPath = System.IO.Path.Combine
                        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]

let goodHints,goodTotalQueries =
                        match contractWithId with
                        | Ok (_,{hints=hints;queries=queries}) -> hints,queries
                        | _ -> failwith "Couldn't make hints or queries"

let sampleContractWithId = match contractWithId with
                           | Ok c -> c
                           | _ -> failwith "Broken sample contract"

let getUTXO _ = UtxoSet.NoOutput
let getTx _ = None
let getWallet _ = Map.empty
let getContractState _ = None

let expectError expectedMsg =
    function
    | Ok _ -> failwithf "\nexpected error msg: %A\nbut was:  Ok" expectedMsg
    | Error wasMsg -> shouldEqual (wasMsg, expectedMsg)

let coinbase blockNumber transactions =
    Block.getBlockCoinbase chain ActiveContractSet.empty blockNumber transactions Hash.zero CGP.empty

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with empty transactions failed validation``(header) =

    let block = { header=header;
                  transactions=[];
                  commitments=[];
                  txMerkleRoot=Hash.zero;
                  witnessMerkleRoot=Hash.zero;
                  activeContractSetMerkleRoot=Hash.zero; }

    Block.validate chain block = Error "transactions is empty"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with invalid header failed validation``(header:BlockHeader) (NonEmptyTransactions transactions) =
    let header = {header with difficulty = 402690497ul}
    let transactions = coinbase header.blockNumber transactions :: transactions

    let block = { header=header;
                  transactions=transactions;
                  commitments=[];
                  txMerkleRoot=Hash.zero;
                  witnessMerkleRoot=Hash.zero;
                  activeContractSetMerkleRoot=Hash.zero; }

    Block.validate chain block = Error "proof of work failed"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with one invalid transaction fail validation``(header) (NonEmptyTransactions transactions) (NonNegativeInt index) =
    let index = 1 + (index % (List.length transactions))
    let header = {header with difficulty = 0x20fffffful }

    let transactions = coinbase header.blockNumber transactions :: transactions

    // Making TX invalid by removing inputs
    let invalidTx =
        {transactions.[index].tx  with inputs = []}
        |> Transaction.toExtended

    let transactions = List.mapi (fun i tx -> if i = index then invalidTx else tx) transactions

    let block = { header=header;
                  transactions=transactions;
                  commitments=[];
                  txMerkleRoot=Hash.zero;
                  witnessMerkleRoot=Hash.zero;
                  activeContractSetMerkleRoot=Hash.zero; }

    let expected = Error (sprintf "transaction %A failed validation due to General \"structurally invalid input(s)\"" invalidTx.txHash)

    Block.validate chain block = expected

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with valid transactions pass validation``(header:BlockHeader) (NonEmptyTransactions transactions) =
    let transactions = coinbase header.blockNumber transactions :: transactions

    let txMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.txHash)
        |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.witnessHash)
        |> MerkleTree.computeRoot

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;Hash.zero;]

    let header = {header with difficulty = 0x20fffffful;commitments=commitments; }

    let block = { header=header;
                  transactions=transactions;
                  commitments=[];
                  txMerkleRoot=txMerkleRoot;
                  witnessMerkleRoot=witnessMerkleRoot;
                  activeContractSetMerkleRoot=Hash.zero }

    Block.validate chain block = Ok block

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block failed when block number is not successive``(parent:BlockHeader) (block:Block) =

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let cgp = CGP.empty
    let ema = EMA.create chain

    parent.blockNumber + 1ul <> block.header.blockNumber
    ==>
    (match Block.connect chain getUTXO getTx contractsPath parent 1UL utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block with
    | Ok _ -> Ok ()
    | Error error -> Error error
    = Error "blockNumber mismatch")

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block should fail when commitments are wrong``(parent:BlockHeader) =

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let cgp = CGP.empty
    let ema = EMA.create chain

    let header = {
        version = parent.version
        parent = Block.hash parent
        blockNumber = parent.blockNumber + 1ul
        commitments = Hash.zero
        timestamp = timestamp
        difficulty = 0x20fffffful
        nonce = 0UL,0UL
    }

    let transactions = [coinbase header.blockNumber []]

    let block = { header=header;
                  transactions=transactions;
                  commitments=[];
                  txMerkleRoot=Hash.zero;
                  witnessMerkleRoot=Hash.zero;
                  activeContractSetMerkleRoot=Hash.zero; }

    match Block.connect chain getUTXO getTx contractsPath parent (timestamp + 1UL) utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block with
    | Ok _ -> Ok ()
    | Error error -> Error error
    = Error "commitments mismatch"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block should fail when transaction inputs are invalid``(parent:BlockHeader) (NonEmptyTransactions transactions) =
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let cgp = CGP.empty
    let ema = EMA.create chain

    let block = Block.createTemplate chain parent timestamp ema acs cgp transactions Hash.zero

    match Block.connect chain getUTXO getTx contractsPath parent (timestamp + 1UL) utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block with
    | Error _ ->
        true
    | _ -> false

[<Test>]
let ``block timestamp too early``() =
    let ema = {
        (EMA.create chain) with delayed = [timestamp-5UL .. timestamp+5UL]
    }

    let rootAccount = createTestAccount()
    let account1, _ = create()
    let tx =
        TestWallet.createTransaction testParameters (publicKeyHash account1) {asset=Asset.Zen;amount=1UL} rootAccount
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let cgp = CGP.empty

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent timestamp ema acs cgp [Transaction.toExtended tx] Hash.zero

    // createTemplate auto-correct to early timestamps, so we have to override the timestamp
    let block = {block with header = {block.header with timestamp = timestamp}}

    Block.connect chain getUTXO getTx contractsPath parent timestamp utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> expectError "block's timestamp is too early"

[<Test>]
let ``block timestamp in the future``() =
    let ema = {
        (EMA.create chain) with delayed = [timestamp-1UL;timestamp; timestamp+1UL]
    }

    let rootAccount = createTestAccount()
    let account1, _ = create()
    let tx =
        TestWallet.createTransaction testParameters (publicKeyHash account1) {asset=Asset.Zen;amount=1UL} rootAccount
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let cgp = CGP.empty

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp + Block.MaxTimeInFuture + 1UL) ema acs cgp [Transaction.toExtended tx] Hash.zero

    Block.connect chain getUTXO getTx contractsPath parent timestamp utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> expectError "block timestamp too far in the future"

[<Test>]
let ``block with mismatch commitments fail connecting``() =
    let rootAccount = createTestAccount()
    let account1, _ = create()
    let tx =
        TestWallet.createTransaction testParameters (publicKeyHash account1) { asset = Asset.Zen; amount = 1UL } rootAccount
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let cgp = CGP.empty
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0x20fffffful;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp + 1UL) ema acs cgp [Transaction.toExtended tx] Hash.zero
    let block = {block with commitments=[Hash.zero]}

    Block.connect chain getUTXO getTx contractsPath parent timestamp utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> expectError "commitments mismatch"

[<Test>]
let ``can connect valid block``() =
    let rootAccount = createTestAccount()
    let account1, _ = create()
    let tx =
        TestWallet.createTransaction testParameters (publicKeyHash account1) { asset = Asset.Zen; amount = 1UL } rootAccount
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let cgp = CGP.empty
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs cgp [Transaction.toExtended tx] Hash.zero

    Block.connect chain getUTXO getTx contractsPath parent timestamp utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> should be ok

[<Test>]
let ``can connect block with coinbase only``() =
    let rootAccount = createTestAccount()
    let account1, _ = create()

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let cgp = CGP.empty
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs cgp [] Hash.zero

    Block.connect chain getUTXO getTx contractsPath parent timestamp utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> should be ok

[<Test>]
[<Parallelizable>]
let ``can connect block with a contract``() =
    let rootAccount = createTestAccount()
    let tx =
        TestWallet.createActivationTransactionFromContract chain sampleContractWithId 1000ul rootAccount
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let contract : Contract.T =
        {
            contractId=SampleContract.sampleContractId
            mainFn = fun tx _ _ _ _ _ _ _ -> Ok (tx,None,Zen.Types.Main.stateUpdate.NoChange)
            costFn = fun _ _ _ _ _ _ _ -> Ok 0L
            expiry = 1000ul
            code=SampleContract.sampleContractCode
        }

    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.contractId contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let cgp = CGP.empty
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain  parent (timestamp+1UL) ema acs cgp [Transaction.toExtended tx] Hash.zero

    Block.connect chain getUTXO getTx contractsPath parent timestamp utxoSet cgp ActiveContractSet.empty ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> should be ok

[<Test>]
let ``block with invalid contract failed connecting``() =
    let rootAccount = createTestAccount() |> fst

    let outpoint = TestWallet.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let output =
        let output = TestWallet.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd
        {output with lock=ActivationSacrifice}

    let tx =
        { version = Version0; contract = Some (V0 { code="ada";hints=goodHints;rlimit=0u;queries=goodTotalQueries }); inputs=[Outpoint outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [ rootKeyPair ] TxHash

    let contract : Contract.T =
        {
            contractId=Contract.makeContractId Version0 "ada"
            mainFn = fun tx _ _ _ _ _ _ _ -> Ok (tx,None,Zen.Types.Main.stateUpdate.NoChange)
            costFn = fun _ _ _ _ _ _ _ -> Ok 0L
            expiry=1000ul
            code=""
        }

    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.contractId contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let cgp = CGP.empty
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs cgp [Transaction.toExtended tx] Hash.zero

    Block.connect chain getUTXO getTx contractsPath parent timestamp utxoSet cgp (ActiveContractSet.empty) ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> expectError "transactions failed inputs validation due to BadContract"

[<Test>]
let ``block with coinbase lock within a regular transaction should fail``() =
    let publicKey = Crypto.PublicKey <| Array.create 64 1uy
    let signature = Crypto.Signature <| Array.create 64 1uy
    let witness = PKWitness (TxHash, publicKey, signature)
    let tx =
        {
            version = Version0
            inputs = [Outpoint {txHash=Hash.zero;index=1ul}];
            outputs=[{lock=Coinbase (15ul,Hash.zero);spend={amount=1UL;asset=Asset.Zen}}]
            witnesses=[witness]
            contract=None
        }

    let transactions  = [coinbase 15ul [Transaction.toExtended tx];Transaction.toExtended tx]

    let txMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.txHash)
        |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.witnessHash)
        |> MerkleTree.computeRoot

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;Hash.zero;]

    let header =
        {
            version = Version0
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = 0x20fffffful;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = { header=header;
                  transactions=transactions;
                  commitments=[];
                  txMerkleRoot=txMerkleRoot;
                  witnessMerkleRoot=witnessMerkleRoot;
                  activeContractSetMerkleRoot=Hash.zero; }

    let expected : Result<Block,string> = Error (sprintf "transaction %A failed validation due to General \"coinbase lock is not allowed within an ordinary transaction\"" <| Transaction.hash tx)

    (Block.validate chain block, expected)
    |> shouldEqual

[<Test>]
let ``block with wrong coinbase reward``() =
    let ema = EMA.create chain
    let parent = {version=0ul; parent=Hash.zero; blockNumber=14ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}

    let tx =
        {
            version = Version0
            inputs = [];
            outputs=[{lock= Coinbase (15ul,Hash.zero);spend={amount=1UL;asset=Asset.Zen}}]
            witnesses=[]
            contract=None
        }

    let transactions  = [Transaction.toExtended tx]

    let txMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.txHash)
        |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.witnessHash)
        |> MerkleTree.computeRoot

    let acsMerkleRoot = ActiveContractSet.root ActiveContractSet.empty

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;acsMerkleRoot]

    let header =
        {
            version = Version0
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = ema.difficulty;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = { header=header;
                  transactions=transactions;
                  commitments=[];
                  txMerkleRoot=txMerkleRoot;
                  witnessMerkleRoot=witnessMerkleRoot;
                  activeContractSetMerkleRoot=acsMerkleRoot }

    Block.connect chain getUTXO getTx contractsPath parent timestamp UtxoSet.asDatabase CGP.empty ActiveContractSet.empty ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> expectError "block reward is incorrect"

[<Test>]
let ``coinbase lock have wrong blockNumber``() =
    let tx =
        {
            version = Version0
            inputs = [];
            outputs=[{lock= Coinbase (13ul,Hash.zero);spend={amount=1UL;asset=Asset.Zen}}]
            witnesses=[]
            contract=None
        }

    let transactions  = [Transaction.toExtended tx]

    let txMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.txHash)
        |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.witnessHash)
        |> MerkleTree.computeRoot

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;Hash.zero;]

    let header =
        {
            version = Version0
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = 0x20fffffful;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = { header=header;
                  transactions=transactions;
                  commitments=[];
                  txMerkleRoot=txMerkleRoot;
                  witnessMerkleRoot=witnessMerkleRoot;
                  activeContractSetMerkleRoot=Hash.zero; }

    let expected : Result<Block,string> =
        Error  "Block failed coinbase validation due to General \"within coinbase transaction all outputs must use coinbase lock\""

    (Block.validate chain block, expected)
    |> shouldEqual

[<Test>]
let ``block without coinbase``() =
    let tx =
        {
            version = Version0
            inputs = [];
            outputs=[{lock= PK Hash.zero;spend={amount=1UL;asset=Asset.Zen}}]
            witnesses=[]
            contract=None
        }

    let transactions  = [Transaction.toExtended tx]

    let txMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.txHash)
        |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.witnessHash)
        |> MerkleTree.computeRoot

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;Hash.zero;]

    let header =
        {
            version = Version0
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = 0x20fffffful;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = { header=header;
                  transactions=transactions;
                  commitments=[];
                  txMerkleRoot=txMerkleRoot;
                  witnessMerkleRoot=witnessMerkleRoot;
                  activeContractSetMerkleRoot=Hash.zero; }

    let expected : Result<Block,string> =
        Error  "Block failed coinbase validation due to General \"within coinbase transaction all outputs must use coinbase lock\""

    (Block.validate chain block, expected)
    |> shouldEqual

[<Test>]
let ``block with coinbase with multiple asset as reward should fail``() =
    let ema = EMA.create chain
    let parent = {version=0ul; parent=Hash.zero; blockNumber=14ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}

    let tx =
        {
            version = Version0
            inputs = [];
            outputs=
                [
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=blockReward 15ul 0uy;asset=Asset.Zen}}
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=1UL;asset=Asset (ContractId (Version0,Hash.Hash (Array.create 32 1uy)), Hash.zero)}}
                ]
            witnesses=[]
            contract=None
        }

    let transactions  = [Transaction.toExtended tx]

    let txMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.txHash)
        |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.witnessHash)
        |> MerkleTree.computeRoot

    let acsMerkleRoot = ActiveContractSet.root ActiveContractSet.empty

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;acsMerkleRoot;]

    let header =
        {
            version = Version0
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = ema.difficulty;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = { header=header;
                  transactions=transactions;
                  commitments=[];
                  txMerkleRoot=txMerkleRoot;
                  witnessMerkleRoot=witnessMerkleRoot;
                  activeContractSetMerkleRoot=acsMerkleRoot; }
    

    Block.connect chain getUTXO getTx contractsPath parent timestamp UtxoSet.asDatabase CGP.empty ActiveContractSet.empty ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> expectError "block reward is incorrect"

[<Test>]
let ``coinbase reward split over multiple outputs``() =
    let ema = EMA.create chain
    let parent = {version=0ul; parent=Hash.zero; blockNumber=14ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}

    let tx =
        {
            version = Version0
            inputs = [];
            outputs=
                [
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=(blockReward 15ul 0uy) / 2UL;asset=Asset.Zen}}
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=(blockReward 15ul 0uy) / 2UL;asset=Asset.Zen}}
                ]
            witnesses=[]
            contract=None
        }

    let transactions  = [Transaction.toExtended tx]

    let txMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.txHash)
        |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map (fun ex -> ex.witnessHash)
        |> MerkleTree.computeRoot

    let acsMerkleRoot = ActiveContractSet.root ActiveContractSet.empty

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;acsMerkleRoot;]

    let header =
        {
            version = Version0
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = 0x20fffffful;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = { header=header;
                  transactions=transactions;
                  commitments=[];
                  txMerkleRoot=txMerkleRoot;
                  witnessMerkleRoot=witnessMerkleRoot;
                  activeContractSetMerkleRoot=acsMerkleRoot; }
     

    Block.connect chain getUTXO getTx contractsPath parent timestamp UtxoSet.asDatabase CGP.empty ActiveContractSet.empty ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> should be ok

[<Test>]
let ``block spending mature transaction is valid``() =

    let rootAccount =
        {fst rootAccountData with blockNumber=100ul}
    let account1, _ = create()

    let origin =
        {
            version = Version0
            inputs=[]
            outputs=[{lock =  Coinbase (1ul, (publicKeyHash rootAccount)); spend= {asset = Asset.Zen;amount=100000000UL}}]
            witnesses=[]
            contract=None
        }
    let originHash = Transaction.hash origin

    let rootAccount = TestWallet.addTransaction originHash origin rootAccount

    let tx =
        TestWallet.createTransaction testParameters (publicKeyHash account1) { asset = Asset.Zen; amount = 1UL } (rootAccount, rootExtendedKey)
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")


    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO originHash origin
    let cgp = CGP.empty
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=100ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs cgp [Transaction.toExtended tx] Hash.zero

    Block.connect chain getUTXO getTx contractsPath parent timestamp utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block
    |>  should be ok

[<Test>]
let ``block spending unmature transaction is invalid``() =
    let rootAccount =
        {fst rootAccountData with blockNumber=Chain.testParameters.coinbaseMaturity}
    let account1, _ = create()

    let origin =
        {
            version = Version0
            inputs=[]
            outputs=[{lock =  Coinbase (1ul, (publicKeyHash rootAccount)); spend= {asset = Asset.Zen;amount=100000000UL}}]
            witnesses=[]
            contract=None
        }
    let originHash = Transaction.hash origin

    let rootAccount = TestWallet.addTransaction originHash origin rootAccount

    let tx =
        TestWallet.createTransaction testParameters (publicKeyHash account1) { asset = Asset.Zen; amount = 1UL } (rootAccount, rootExtendedKey)
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")


    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO originHash origin
    let cgp = CGP.empty
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=Chain.testParameters.coinbaseMaturity - 1ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs cgp [Transaction.toExtended tx] Hash.zero

    Block.connect chain getUTXO getTx contractsPath parent timestamp utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> expectError "transactions failed inputs validation due to General \"Coinbase not mature enough\""

[<Test>]
let ``contract get removed when expiring arrive``() =
    let contract : Contract.T =
        {
            contractId = SampleContract.sampleContractId
            mainFn = fun tx _ _ _ _ _ _ _ -> Ok (tx,None,Zen.Types.Main.stateUpdate.NoChange)
            costFn = fun _ _ _ _ _ _ _ -> Ok 0L
            expiry = 0ul
            code = ""
        }

    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.contractId contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let cgp = CGP.empty
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs cgp [] Hash.zero

    Block.connect chain getUTXO getTx contractsPath parent timestamp utxoSet cgp acs ContractCache.empty ema getContractState ContractStates.asDatabase block
    |> should be ok

//TODO: transaction validation was fixed and now needs a valid transaction - need to redo the test
//[<Test>]
//let ``Overweight block should be rejected``() =
//    let rootAccount = createTestAccount()
//    let cLock = Lock.Contract (ContractId (Version0,Hash.zero))
//    let cWitness = {
//        contractId = SampleContract.sampleContractId;
//        command = "nothing";
//        messageBody = None;
//        stateCommitment = NotCommitted;
//        beginInputs = 0u;
//        beginOutputs = 0u;
//        inputsLength = 0u;
//        outputsLength = 0u;
//        signature = None
//        cost = uint64 chain.maxBlockWeight + 1UL // Weight >>> max block weight
//    }
//
//    let spend = { asset = Asset.Zen; amount = 1UL }
//    let tx1 =
//        Account.createTransactionFromLock cLock spend rootAccount
//        |> (function | Ok x -> x | _ -> failwith "failed transaction generation")
//
//    // find the exact output
//    let i, _ =
//        tx1.outputs
//        |> List.mapi (fun i t -> i, t)
//        |> List.find (fun (_, t) -> match t.lock with | Contract _ -> true | _ -> false)
//
//    let keyPair = Crypto.KeyPair.create()
//
//    let mintSpend = { asset = Asset (SampleContract.sampleContractId, Hash.zero); amount = 1UL }
//
//    let tx1Hash = Transaction.hash tx1
//    let tx2 =
//        {
//            version = Version0
//            inputs = [ Outpoint {txHash=tx1Hash;index=uint32 i}; Mint mintSpend ]
//            outputs = [ { lock = PK Hash.zero; spend = spend }; { lock = PK Hash.zero; spend = mintSpend } ]
//            witnesses = [];
//            contract = None;
//        }
//        |> Transaction.sign [ keyPair ]
//        |> Transaction.addWitnesses [ ContractWitness cWitness ]
//
//    let contract : Contract.T =
//        {
//            contractId = SampleContract.sampleContractId
//            mainFn = fun tx _ _ _ _ _ _ _ -> Ok (tx,None,Zen.Types.Main.stateUpdate.NoChange)
//            costFn = fun _ _ _ _ _ _ _ -> int64 chain.maxBlockWeight + 1L
//            expiry = 0ul
//            code = ""
//        }
//
//    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.contractId contract
//    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
//    let ema = EMA.create chain
//
//    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
//    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs [tx1;tx2] Hash.zero
//
//    let res = Block.connect chain getUTXO contractsPath parent timestamp utxoSet acs ContractCache.empty ema getContractState ContractStates.asDatabase block
//    res |> should not' (be ok)
//    let errStr =
//        match res with | Error err -> err | _ -> assert false; ""
//    errStr |> should contain "block weight exceeds maximum"
