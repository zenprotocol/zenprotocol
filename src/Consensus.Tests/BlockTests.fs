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
open Consensus.Tests.SampleContract
open Helper

let timestamp = 1515594186383UL + 1UL
let difficulty = 0x20fffffful

let chain = getChainParameters Chain.Local

let contractsPath = "./test"

let goodHints = match Contract.recordHints sampleContractCode with
                | Ok hints -> hints
                | _ -> failwith "Couldn't make hints"

let goodTotalQueries = match Infrastructure.ZFStar.totalQueries goodHints with
                       | Ok totalQueries -> totalQueries
                       | _ -> failwith "Couldn't get total queries"

let getUTXO _ = UtxoSet.NoOutput
let getWallet _ = Map.empty

type BkResult = Result<(Block*UtxoSet.T*ActiveContractSet.T*ContractCache.T*EMA.T) , string>

let coinbase blockNumber transactions =
    Block.getBlockCoinbase chain ActiveContractSet.empty blockNumber transactions Hash.zero

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with empty transactions failed validation``(header) =

    let block = {header=header;transactions=[];commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}

    Block.validate chain block = Error "transactions is empty"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with invalid header failed validation``(header:BlockHeader) (NonEmptyTransactions transactions) =
    let header = {header with difficulty = 402690497ul}
    let transactions = coinbase header.blockNumber transactions :: transactions

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}

    Block.validate chain block = Error "proof of work failed"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with one invalid transaction fail validation``(header) (NonEmptyTransactions transactions) (NonNegativeInt index) =
    let index = 1 + (index % (List.length transactions))
    let header = {header with difficulty = 0x20fffffful }

    let transactions = coinbase header.blockNumber transactions :: transactions

    // Making TX invalid by removing inputs
    let invalidTx = {transactions.[index] with inputs = []}

    let transactions = List.mapi (fun i tx -> if i = index then invalidTx else tx) transactions

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}

    let expected = Error (sprintf "transaction %A failed validation due to General \"inputs empty\"" (Transaction.hash invalidTx))

    Block.validate chain block = expected

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with valid transactions pass validation``(header) (NonEmptyTransactions transactions) =
    let transactions = coinbase header.blockNumber transactions :: transactions

    let txMerkleRoot =
        transactions
        |> List.map Transaction.hash
        |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map Transaction.witnessHash
        |> MerkleTree.computeRoot

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;Hash.zero;]

    let header = {header with difficulty = 0x20fffffful;commitments=commitments; }

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=txMerkleRoot; witnessMerkleRoot=witnessMerkleRoot;activeContractSetMerkleRoot=Hash.zero;}

    Block.validate chain block = Ok block

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block failed when block number is not successive``(parent:BlockHeader) (block:Block) =

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let ema = EMA.create chain

    parent.blockNumber + 1ul <> block.header.blockNumber
    ==>
    (match Block.connect chain getUTXO contractsPath parent 1UL utxoSet acs ContractCache.empty ema block with
    | Ok _ -> Ok ()
    | Error error -> Error error
    = Error "blockNumber mismatch")

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block should fail when commitments are wrong``(parent:BlockHeader) =

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
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

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}

    match Block.connect chain getUTXO contractsPath parent (timestamp + 1UL) utxoSet acs ContractCache.empty ema block with
    | Ok _ -> Ok ()
    | Error error -> Error error
    = Error "commitments mismatch"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block should fail when transaction inputs are invalid``(parent:BlockHeader) (NonEmptyTransactions transactions) =
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let ema = EMA.create chain

    let block = Block.createTemplate chain parent timestamp ema acs transactions Hash.zero

    match Block.connect chain getUTXO contractsPath parent (timestamp + 1UL) utxoSet acs ContractCache.empty ema block with
    | Error error
                when error.StartsWith "transactions failed inputs validation due to"
                  || error = "Output lookup error"
                  || error.Contains "Orphan"
                  || error.Contains "witnesses" -> true     //too many/few wts
    | Error _ as res ->
        System.Console.WriteLine (sprintf "Got unexpected result %A" res)
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
        Account.createTransaction (publicKeyHash account1) {asset=Constants.Zen;amount=1UL} rootAccount
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent timestamp ema acs [tx] Hash.zero

    let expected : BkResult = Error "block's timestamp is too early"

    Block.connect chain getUTXO contractsPath parent timestamp utxoSet acs ContractCache.empty ema block
    |> should equal expected

[<Test>]
let ``block timestamp in the future``() =
    let ema = {
        (EMA.create chain) with delayed = [timestamp-1UL;timestamp; timestamp+1UL]
    }

    let rootAccount = createTestAccount()
    let account1, _ = create()
    let tx =
        Account.createTransaction (publicKeyHash account1) {asset=Constants.Zen;amount=1UL} rootAccount
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp + Block.MaxTimeInFuture + 1UL) ema acs [tx] Hash.zero

    let expected : BkResult = Error "block timestamp too far in the future"

    Block.connect chain getUTXO contractsPath parent timestamp utxoSet acs ContractCache.empty ema block
    |> should equal expected

[<Test>]
let ``block with mismatch commitments fail connecting``() =
    let rootAccount = createTestAccount()
    let account1, _ = create()
    let tx =
        Account.createTransaction (publicKeyHash account1) { asset = Constants.Zen; amount = 1UL } rootAccount
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0x20fffffful;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp + 1UL) ema acs [tx] Hash.zero
    let block = {block with commitments=[Hash.zero]}

    let expected : BkResult = Error "commitments mismatch"

    Block.connect chain getUTXO contractsPath parent timestamp utxoSet acs ContractCache.empty ema block
    |> should equal expected

[<Test>]
let ``can connect valid block``() =
    let rootAccount = createTestAccount()
    let account1, _ = create()
    let tx =
        Account.createTransaction (publicKeyHash account1) { asset = Constants.Zen; amount = 1UL } rootAccount
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs [tx] Hash.zero

    Block.connect chain getUTXO contractsPath parent timestamp utxoSet acs ContractCache.empty ema block
    |> should be ok

[<Test>]
let ``can connect block with coinbase only``() =
    let rootAccount = createTestAccount()
    let account1, _ = create()

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs [] Hash.zero

    Block.connect chain getUTXO contractsPath parent timestamp utxoSet acs ContractCache.empty ema block
    |> should be ok

[<Test>]
[<ParallelizableAttribute>]
let ``can connect block with a contract``() =
    let rootAccount = createTestAccount()
    let tx =
        Account.createActivateContractTransaction chain SampleContract.sampleContractCode 1000ul rootAccount
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let contract : Contract.T =
        {
            hash=Contract.computeHash SampleContract.sampleContractCode
            mainFn = fun tx _ _ _ _ _ -> Ok (tx,None)
            costFn = fun _ _ _ _ _ -> 0L
            expiry=1001ul
            code=SampleContract.sampleContractCode
        }

    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.hash contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain  parent (timestamp+1UL) ema acs [tx] Hash.zero

    Block.connect chain getUTXO contractsPath parent timestamp utxoSet ActiveContractSet.empty ContractCache.empty ema block
    |> should be ok

[<Test>]
let ``block with invalid contract failed connecting``() =
    let rootAccount = createTestAccount() |> fst

    let outpoint = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> fst
    let output =
        let output = Account.getUnspentOutputs rootAccount |> fst |> Map.toSeq |> Seq.head |> snd
        {output with lock=ActivationSacrifice}

    let tx =
        {contract = Some { code="ada";hints=goodHints;rlimit=0u;queries=goodTotalQueries }; inputs=[Outpoint outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [ rootKeyPair ]

    let contract : Contract.T =
        {
            hash=Contract.computeHash "ada"
            mainFn = fun tx _ _ _ _ _ -> Ok (tx,None)
            costFn = fun _ _ _ _ _ -> 0L
            expiry=1000ul
            code=""
        }

    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.hash contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs [tx] Hash.zero

    let expected : BkResult = Error "transactions failed inputs validation due to BadContract"

    Block.connect chain getUTXO contractsPath parent timestamp utxoSet (ActiveContractSet.empty) ContractCache.empty ema block
    |> should equal expected

[<Test>]
let ``block with coinbase lock within a regular transaction should fail``() =
    let tx =
        {
            inputs = [Outpoint {txHash=Hash.zero;index=1ul}];
            outputs=[{lock= Coinbase (15ul,Hash.zero);spend={amount=1UL;asset=Constants.Zen}}]
            witnesses=[]
            contract=None
        }

    let transactions  = [coinbase 15ul [tx];tx]

    let txMerkleRoot =
            transactions
            |> List.map Transaction.hash
            |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map Transaction.witnessHash
        |> MerkleTree.computeRoot

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;Hash.zero;]

    let header =
        {
            version = Block.Version
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = 0x20fffffful;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=txMerkleRoot; witnessMerkleRoot=witnessMerkleRoot;activeContractSetMerkleRoot=Hash.zero;}

    let expected : Result<Block,string> = Error (sprintf "transaction %A failed validation due to General \"coinbase lock is not allowed within an ordinary transaction\"" <| Transaction.hash tx)

    Block.validate chain block |> should equal expected

[<Test>]
let ``block with wrong coinbase reward``() =
    let ema = EMA.create chain
    let parent = {version=0ul; parent=Hash.zero; blockNumber=14ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}

    let tx =
        {
            inputs = [];
            outputs=[{lock= Coinbase (15ul,Hash.zero);spend={amount=1UL;asset=Constants.Zen}}]
            witnesses=[]
            contract=None
        }

    let transactions  = [tx]

    let txMerkleRoot =
            transactions
            |> List.map Transaction.hash
            |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map Transaction.witnessHash
        |> MerkleTree.computeRoot

    let acsMerkleRoot = SparseMerkleTree.root ActiveContractSet.empty

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;acsMerkleRoot]

    let header =
        {
            version = Block.Version
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = ema.difficulty;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=txMerkleRoot; witnessMerkleRoot=witnessMerkleRoot;activeContractSetMerkleRoot=acsMerkleRoot;}

    let expected : BkResult = Error "block reward is incorrect"

    Block.connect chain getUTXO contractsPath parent timestamp (UtxoSet.asDatabase) (ActiveContractSet.empty) ContractCache.empty ema block
    |> should equal expected

[<Test>]
let ``coinbase lock have wrong blockNumber``() =
    let tx =
        {
            inputs = [];
            outputs=[{lock= Coinbase (13ul,Hash.zero);spend={amount=1UL;asset=Constants.Zen}}]
            witnesses=[]
            contract=None
        }

    let transactions  = [tx]

    let txMerkleRoot =
            transactions
            |> List.map Transaction.hash
            |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map Transaction.witnessHash
        |> MerkleTree.computeRoot

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;Hash.zero;]

    let header =
        {
            version = Block.Version
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = 0x20fffffful;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=txMerkleRoot; witnessMerkleRoot=witnessMerkleRoot;activeContractSetMerkleRoot=Hash.zero;}

    let expected : Result<Block,string> =
        Error  "Block failed coinbase validation due to General \"within coinbase transaction all outputs must use coinbase lock\""

    Block.validate chain block |> should equal expected

[<Test>]
let ``block without coinbase``() =
    let tx =
        {
            inputs = [];
            outputs=[{lock= PK Hash.zero;spend={amount=1UL;asset=Constants.Zen}}]
            witnesses=[]
            contract=None
        }

    let transactions  = [tx]

    let txMerkleRoot =
            transactions
            |> List.map Transaction.hash
            |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map Transaction.witnessHash
        |> MerkleTree.computeRoot

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;Hash.zero;]

    let header =
        {
            version = Block.Version
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = 0x20fffffful;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=txMerkleRoot; witnessMerkleRoot=witnessMerkleRoot;activeContractSetMerkleRoot=Hash.zero;}

    let expected : Result<Block,string> =
        Error  "Block failed coinbase validation due to General \"within coinbase transaction all outputs must use coinbase lock\""

    Block.validate chain block |> should equal expected

[<Test>]
let ``block with coinbase with multiple asset as reward should fail``() =
    let ema = EMA.create chain
    let parent = {version=0ul; parent=Hash.zero; blockNumber=14ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}

    let tx =
        {
            inputs = [];
            outputs=
                [
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=Block.blockReward 15ul;asset=Constants.Zen}}
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=1UL;asset=Hash.Hash (Array.create 32 1uy), Hash.zero}}
                ]
            witnesses=[]
            contract=None
        }

    let transactions  = [tx]

    let txMerkleRoot =
            transactions
            |> List.map Transaction.hash
            |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map Transaction.witnessHash
        |> MerkleTree.computeRoot

    let acsMerkleRoot = SparseMerkleTree.root ActiveContractSet.empty

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;acsMerkleRoot;]

    let header =
        {
            version = Block.Version
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = ema.difficulty;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=txMerkleRoot; witnessMerkleRoot=witnessMerkleRoot;activeContractSetMerkleRoot=acsMerkleRoot;}

    let expected : BkResult =
        Error "block reward is incorrect"

    Block.connect chain getUTXO contractsPath parent timestamp (UtxoSet.asDatabase) (ActiveContractSet.empty) ContractCache.empty ema block
    |> should equal expected

[<Test>]
let ``coinbase reward split over multiple outputs``() =
    let ema = EMA.create chain
    let parent = {version=0ul; parent=Hash.zero; blockNumber=14ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}

    let tx =
        {
            inputs = [];
            outputs=
                [
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=(Block.blockReward 15ul) / 2UL;asset=Constants.Zen}}
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=(Block.blockReward 15ul) / 2UL;asset=Constants.Zen}}
                ]
            witnesses=[]
            contract=None
        }

    let transactions  = [tx]

    let txMerkleRoot =
            transactions
            |> List.map Transaction.hash
            |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map Transaction.witnessHash
        |> MerkleTree.computeRoot

    let acsMerkleRoot = SparseMerkleTree.root ActiveContractSet.empty

    let commitments = MerkleTree.computeRoot [txMerkleRoot;witnessMerkleRoot;acsMerkleRoot;]

    let header =
        {
            version = Block.Version
            parent = Hash.zero
            blockNumber = 15ul
            difficulty = 0x20fffffful;
            commitments=commitments;
            timestamp = timestamp
            nonce = 0UL,0UL
        }

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=txMerkleRoot; witnessMerkleRoot=witnessMerkleRoot;activeContractSetMerkleRoot=acsMerkleRoot;}

    Block.connect chain getUTXO contractsPath parent timestamp (UtxoSet.asDatabase) (ActiveContractSet.empty) ContractCache.empty ema block
    |> should be ok

[<Test>]
let ``block spending mature transaction is valid``() =

    let rootAccount =
        {fst rootAccountData with blockNumber=100ul}
    let account1, _ = create()

    let origin =
        {
            inputs=[]
            outputs=[{lock =  Coinbase (1ul, (publicKeyHash rootAccount)); spend= {asset = Constants.Zen;amount=100000000UL}}]
            witnesses=[]
            contract=None
        }
    let originHash = Transaction.hash origin

    let rootAccount = Account.addTransaction originHash origin rootAccount

    let tx =
        Account.createTransaction (publicKeyHash account1) { asset = Constants.Zen; amount = 1UL } (rootAccount, rootExtendedKey)
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")


    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO originHash origin
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=100ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs [tx] Hash.zero

    Block.connect chain getUTXO contractsPath parent timestamp utxoSet acs ContractCache.empty ema block
    |>  should be ok


[<Test>]
let ``block spending unmature transaction is invalid``() =
    let rootAccount =
        {fst rootAccountData with blockNumber=100ul}
    let account1, _ = create()

    let origin =
        {
            inputs=[]
            outputs=[{lock =  Coinbase (1ul, (publicKeyHash rootAccount)); spend= {asset = Constants.Zen;amount=100000000UL}}]
            witnesses=[]
            contract=None
        }
    let originHash = Transaction.hash origin

    let rootAccount = Account.addTransaction originHash origin rootAccount

    let tx =
        Account.createTransaction (publicKeyHash account1) { asset = Constants.Zen; amount = 1UL } (rootAccount, rootExtendedKey)
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")


    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO originHash origin
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=99ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs [tx] Hash.zero

    let expected : BkResult =
        Error "transactions failed inputs validation due to General \"Coinbase not mature enough\""

    Block.connect chain getUTXO contractsPath parent timestamp utxoSet acs ContractCache.empty ema block
    |> should equal expected

[<Test>]
let ``contract get removed when expiring arrive``() =
    let contract : Contract.T =
        {
            hash=Contract.computeHash SampleContract.sampleContractCode
            mainFn= fun tx _ _ _ _ _ -> Ok (tx,None)
            costFn = fun _ _ _ _ _ -> 0L
            expiry=1ul
            code=""
        }

    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.hash contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs [] Hash.zero

    Block.connect chain getUTXO contractsPath parent timestamp utxoSet acs ContractCache.empty ema block
    |> should be ok

[<Test>]
let ``Overweight block should be rejected``() =
    let rootAccount = createTestAccount()
    let cLock = Lock.Contract Hash.zero
    let cWitness = {
        cHash = Hash.zero;
        command = "nothing";
        data = None;
        beginInputs = 0u;
        beginOutputs = 0u;
        inputsLength = 1u;
        outputsLength = 0u;
        signature = None
        cost = System.UInt32.MaxValue       // Weight >>> max block weight
        }
    let tx1 =
        Account.createTransactionFromLock cLock { asset = Constants.Zen; amount = 1UL } rootAccount
        |> (function | Ok x -> x | _ -> failwith "failed transaction generation")

    // find the exact output
    let i, _ =
        tx1.outputs
        |> List.mapi (fun i t -> i, t)
        |> List.find (fun (_, t) -> match t.lock with | Contract _ -> true | _ -> false)

    let tx1Hash = Transaction.hash tx1
    let tx2 = {
        inputs = [Outpoint {txHash=tx1Hash;index=uint32 i}];
        outputs = [];
        witnesses = [ContractWitness cWitness];
        contract = None;
    }

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO rootTxHash rootTx
    let ema = EMA.create chain

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate chain parent (timestamp+1UL) ema acs [tx1;tx2] Hash.zero

    let res = Block.connect chain getUTXO contractsPath parent timestamp utxoSet acs ContractCache.empty ema block
    res |> should not' (be ok)
    let errStr =
        match res with | Error err -> err | _ -> assert false; ""
    errStr |> should contain "greater than maximum block weight"
