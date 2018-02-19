module Consensus.Tests.BlockTests

open Consensus
open Consensus.ChainParameters
open Consensus.Types
open Consensus.Tests
open Wallet
open NUnit.Framework
open Infrastructure
open FsCheck
open FsCheck.NUnit
open FsUnit
open TestsInfrastructure.Constraints

let timestamp = 1515594186383UL + 1UL
let difficulty = 0x20fffffful

let contractsPath = "./test"

let getUTXO _ = UtxoSet.NoOutput
let getWallet _ = Map.empty

let coinbase blockNumber transactions =
    Block.getBlockCoinbase blockNumber transactions Hash.zero

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with empty transactions failed validation``(header) =

    let block = {header=header;transactions=[];commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}

    Block.validate Chain.Local block = Error "transactions is empty"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with invalid header failed validation``(header:BlockHeader) (NonEmptyTransactions transactions) =
    let header = {header with difficulty = 402690497ul}
    let transactions = coinbase header.blockNumber transactions :: transactions

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}

    Block.validate Chain.Local block = Error "proof of work failed"

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

    Block.validate Chain.Local block = expected

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

    Block.validate Chain.Local block = Ok block

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block failed when block number is not successive``(parent:BlockHeader) (block:Block) =

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let ema = EMA.create Chain.Local

    parent.blockNumber + 1ul <> block.header.blockNumber
    ==> (Block.connect Chain.Local getUTXO contractsPath parent 1UL utxoSet acs ema block = Error "blockNumber mismatch")

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block should fail when commitments are wrong``(parent:BlockHeader) =

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let ema = EMA.create Chain.Local

    let header = {
        version = parent.version
        parent = BlockHeader.hash parent
        blockNumber = parent.blockNumber + 1ul
        commitments = Hash.zero
        timestamp = timestamp
        difficulty = 0x20fffffful
        nonce = 0UL,0UL
    }

    let transactions = [coinbase header.blockNumber []]

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}

    Block.connect Chain.Local getUTXO contractsPath parent (timestamp + 1UL) utxoSet acs ema block = Error "commitments mismatch"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block should fail when transaction inputs are invalid``(parent:BlockHeader) (NonEmptyTransactions transactions) =
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let ema = EMA.create Chain.Local

    let block = Block.createTemplate parent timestamp ema acs transactions Hash.zero

    Block.connect Chain.Local getUTXO contractsPath parent (timestamp + 1UL) utxoSet acs ema block =
        Error "transactions failed inputs validation due to Orphan"

[<Test>]
let ``block timestamp too early``() =
    let ema = {
        (EMA.create Chain.Local) with delayed = [timestamp-5UL .. timestamp+5UL]
    }

    let rootAccount = Account.createTestAccount ()
    let account1 = Account.create ()
    let tx =
        Account.createTransaction Chain.Local rootAccount account1.publicKeyHash {asset=Constants.Zen;amount=1UL}
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent timestamp ema acs [tx] Hash.zero

    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T) , string> = Error "block's timestamp is too early"

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should equal expected

[<Test>]
let ``block timestamp in the future``() =
    let ema = {
        (EMA.create Chain.Local) with delayed = [timestamp-1UL;timestamp; timestamp+1UL]
    }

    let rootAccount = Account.createTestAccount ()
    let account1 = Account.create ()
    let tx =
        Account.createTransaction Chain.Local rootAccount account1.publicKeyHash {asset=Constants.Zen;amount=1UL}
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp + Block.MaxTimeInFuture + 1UL) ema acs [tx] Hash.zero

    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T) , string> = Error "block timestamp too far in the future"

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should equal expected

[<Test>]
let ``block with mismatch commitments fail connecting``() =
    let rootAccount = Account.createTestAccount ()
    let account1 = Account.create ()
    let tx =
        Account.createTransaction Chain.Local rootAccount account1.publicKeyHash { asset = Constants.Zen; amount = 1UL }
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Local

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0x20fffffful;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp + 1UL) ema acs [tx] Hash.zero
    let block = {block with commitments=[Hash.zero]}

    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T) , string> = Error "commitments mismatch"

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should equal expected

[<Test>]
let ``can connect valid block``() =
    let rootAccount = Account.createTestAccount ()
    let account1 = Account.create ()
    let tx =
        Account.createTransaction Chain.Local rootAccount account1.publicKeyHash { asset = Constants.Zen; amount = 1UL }
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Local

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [tx] Hash.zero

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should be ok

[<Test>]
let ``can connect block with coinbase only``() =
    let rootAccount = Account.createTestAccount ()
    let account1 = Account.create ()

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Local

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [] Hash.zero

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should be ok

[<Test>]
let ``can connect block with a contract``() =
    let rootAccount = Account.createTestAccount ()
    let tx =
        Account.createActivateContractTransaction rootAccount SampleContract.sampleContractCode
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")

    let contract : Contract.T =
        {
            hash=Contract.computeHash SampleContract.sampleContractCode
            fn= fun _ _ _ _ tx -> Ok (tx,None)
            costFn = fun _ _ _ _ -> Ok 0I
            expiry=1001ul
            size=100ul
        }

    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.hash contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Local

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [tx] Hash.zero

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet ActiveContractSet.empty ema block
    |> should be ok

[<Test>]
let ``block with invalid contract failed connecting``() =
    let rootAccount = Account.createTestAccount ()

    let outpoint = Account.getUnspentOutputs rootAccount |> Map.toSeq |> Seq.head |> fst
    let output = Account.getUnspentOutputs rootAccount |> Map.toSeq |> Seq.head |> snd

    let tx =
        {contract = Some ("ada","dasdas"); inputs=[outpoint]; outputs=[output];witnesses=[]}
        |> Transaction.sign [rootAccount.keyPair]

    let contract : Contract.T =
        {
            hash=Contract.computeHash "ada"
            fn= fun _ _ _ _ tx -> Ok (tx,None)
            costFn = fun _ _ _ _ -> Ok 0I
            expiry=1000ul
            size=100ul
        }

    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.hash contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Local

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [tx] Hash.zero

    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T) , string> = Error "transactions failed inputs validation due to BadContract"

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet (ActiveContractSet.empty) ema block
    |> should equal expected

[<Test>]
let ``block with coinbase lock within a regular transaction should fail``() =
    let tx =
        {
            inputs = [{txHash=Hash.zero;index=1ul}];
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

    let expected : Result<Block,string> = Error (sprintf "transaction %A failed validation due to General \"coinbase lock is not allow within regular transaction\"" <| Transaction.hash tx)

    Block.validate Chain.Local block |> should equal expected

[<Test>]
let ``block with wrong coinbase reward``() =
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

    let expected : Result<Block,string> = Error "block reward is incorrect"

    Block.validate Chain.Local block |> should equal expected

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

    Block.validate Chain.Local block |> should equal expected

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

    Block.validate Chain.Local block |> should equal expected

[<Test>]
let ``block with coinbase with multiple asset as reward should fail``() =
    let tx =
        {
            inputs = [];
            outputs=
                [
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=Block.getBlockReward 15ul;asset=Constants.Zen}}
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
        Error "block reward is incorrect"

    Block.validate Chain.Local block |> should equal expected

[<Test>]
let ``coinbase reward split over multiple outputs``() =
    let tx =
        {
            inputs = [];
            outputs=
                [
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=(Block.getBlockReward 15ul) / 2UL;asset=Constants.Zen}}
                    {lock= Coinbase (15ul,Hash.zero);spend={amount=(Block.getBlockReward 15ul) / 2UL;asset=Constants.Zen}}
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

    let expected : Result<Block,string> = Ok block

    Block.validate Chain.Local block |> should equal expected

[<Test>]
let ``block spending mature transaction is valid``() =
    let rootAccount =
        {Account.rootAccount with blockNumber=100ul}
    let account1 = Account.create ()

    let origin =
        {
            inputs=[]
            outputs=[{lock =  Coinbase (1ul, rootAccount.publicKeyHash); spend= {asset = Constants.Zen;amount=100000000UL}}]
            witnesses=[]
            contract=None
        }
    let originHash = Transaction.hash origin

    let rootAccount = Account.addTransaction originHash origin rootAccount

    let tx =
        Account.createTransaction Chain.Local rootAccount account1.publicKeyHash { asset = Constants.Zen; amount = 1UL }
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")


    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO originHash origin
    let ema = EMA.create Chain.Local

    let parent = {version=0ul; parent=Hash.zero; blockNumber=100ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [tx] Hash.zero

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should be ok


[<Test>]
let ``block spending unmature transaction is invalid``() =
    let rootAccount =
        {Account.rootAccount with blockNumber=100ul}
    let account1 = Account.create ()

    let origin =
        {
            inputs=[]
            outputs=[{lock =  Coinbase (1ul, rootAccount.publicKeyHash); spend= {asset = Constants.Zen;amount=100000000UL}}]
            witnesses=[]
            contract=None
        }
    let originHash = Transaction.hash origin

    let rootAccount = Account.addTransaction originHash origin rootAccount

    let tx =
        Account.createTransaction Chain.Local rootAccount account1.publicKeyHash { asset = Constants.Zen; amount = 1UL }
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")


    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO originHash origin
    let ema = EMA.create Chain.Local

    let parent = {version=0ul; parent=Hash.zero; blockNumber=99ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [tx] Hash.zero

    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T) , string> =
        Error "transactions failed inputs validation due to General \"Coinbase not mature enough\""

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should equal expected

[<Test>]
let ``contract get removed when expiring arrive``() =
    let contract : Contract.T =
        {
            hash=Contract.computeHash SampleContract.sampleContractCode
            fn= fun _ _ _ _ tx -> Ok (tx,None)
            costFn = fun _ _ _ _ -> Ok 0I
            expiry=1ul
            size=100ul
        }

    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.hash contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Local

    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [] Hash.zero

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should be ok