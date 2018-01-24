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

let timestamp = 1515594186383UL + 1UL
let difficulty = 0x20fffffful

let contractsPath = "./test"

let getUTXO _ = UtxoSet.NoOuput
let getWallet _ = Map.empty
 
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with empty transactions failed validation``(header) =
    
    let block = {header=header;transactions=[];commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;} 

    Block.validate Chain.Test block = Error "transactions is empty"
  
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``block with invalid header failed validation``(header:BlockHeader) (NonEmptyTransactions transactions) =
    let header = {header with difficulty = 402690497ul}

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}
        
    Block.validate Chain.Test block = Error "proof of work failed"
    
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``block with one invalid transaction fail validation``(header) (NonEmptyTransactions transactions) (NonNegativeInt index) =
    let index = index % (List.length transactions) 
    let header = {header with difficulty = 0x20fffffful }
    
    // Making TX invalid by removing inputs
    let invalidTx = {transactions.[index] with inputs = []}
    let transactions = List.mapi (fun i tx -> if i = index then invalidTx else tx) transactions
             
    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}
    
    let expected = Error (sprintf "transaction %A failed validation due to General \"inputs empty\"" (Transaction.hash invalidTx))

    Block.validate Chain.Test block = expected
    
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``block with valid transactions pass validation``(header) (NonEmptyTransactions transactions) =
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
        
    Block.validate Chain.Test block = Ok block
    
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]        
let ``connecting block failed when block number is not successive``(parent:BlockHeader) (block:Block) =

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let contractWallets = ContractWallets.asDatabase
    let ema = EMA.create Chain.Test

    parent.blockNumber + 1ul <> block.header.blockNumber 
    ==> (Block.connect Chain.Test getUTXO getWallet contractsPath parent 1UL utxoSet acs ema contractWallets block = Error "blockNumber mismatch")  

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block should fail when commitments are wrong``(parent:BlockHeader) (NonEmptyTransactions transactions) = 
    
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let contractWallets = ContractWallets.asDatabase
    let ema = EMA.create Chain.Test   
    
    let header = {
        version = parent.version
        parent = BlockHeader.hash parent
        blockNumber = parent.blockNumber + 1ul
        commitments = Hash.zero
        timestamp = timestamp
        difficulty = 0x20fffffful
        nonce = 0UL,0UL        
    }
    
    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}
    
    Block.connect Chain.Test getUTXO getWallet contractsPath parent (timestamp + 1UL) utxoSet acs ema contractWallets block = Error "commitments mismatch"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``connecting block should fail when transaction inputs are invalid``(parent:BlockHeader) (NonEmptyTransactions transactions) =
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let contractWallets = ContractWallets.asDatabase
    let ema = EMA.create Chain.Test   
 
    let block = Block.createTemplate parent timestamp ema acs transactions
        
    Block.connect Chain.Test getUTXO getWallet contractsPath parent (timestamp + 1UL) utxoSet acs ema contractWallets block = 
        Error "transactions failed inputs validation due to Orphan"

[<Test>]    
let ``block timestamp too early``() =
    let ema = {
        (EMA.create Chain.Test) with delayed = [timestamp-5UL .. timestamp+5UL]
    }
        
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let tx = 
        Account.createTransaction Chain.Test rootAccount account1.publicKeyHash {asset=Hash.zero;amount=1UL}
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation") 
    
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx    
    let contractWallets = ContractWallets.asDatabase
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent timestamp ema acs [tx]
    
    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T*ContractWallets.T) , string> = Error "block's timestamp is too early"

    Block.connect Chain.Test getUTXO getWallet contractsPath parent timestamp utxoSet acs ema contractWallets block 
    |> should equal expected
    
[<Test>]    
let ``block timestamp in the future``() =
    let ema = {
        (EMA.create Chain.Test) with delayed = [timestamp-1UL;timestamp; timestamp+1UL]
    }
        
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let tx = 
        Account.createTransaction Chain.Test rootAccount account1.publicKeyHash {asset=Hash.zero;amount=1UL}
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation") 
    
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx    
    let contractWallets = ContractWallets.asDatabase
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp + Block.MaxTimeInFuture + 1UL) ema acs [tx]
    
    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T*ContractWallets.T) , string> = Error "block timestamp too far in the future"
   
    Block.connect Chain.Test getUTXO getWallet contractsPath parent timestamp utxoSet acs ema contractWallets block 
    |> should equal expected

[<Test>]    
let ``block with mismatch commitments fail connecting``() = 
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let tx = 
        Account.createTransaction Chain.Test rootAccount account1.publicKeyHash { asset = Hash.zero; amount = 1UL }
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation") 
    
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let contractWallets = ContractWallets.asDatabase
    let ema = EMA.create Chain.Test
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0x20fffffful;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp + 1UL) ema acs [tx]
    let block = {block with commitments=[Hash.zero]}
    
    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T*ContractWallets.T) , string> = Error "commitments mismatch"
    
    Block.connect Chain.Test getUTXO getWallet contractsPath parent timestamp utxoSet acs ema contractWallets block 
    |> should equal expected
    
[<Test>]    
let ``can connect valid block``() = 
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let tx = 
        Account.createTransaction Chain.Test rootAccount account1.publicKeyHash { asset = Hash.zero; amount = 1UL }
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation") 
    
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let contractWallets = ContractWallets.asDatabase
    let ema = EMA.create Chain.Test
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [tx]
    
    let result = Block.connect Chain.Test getUTXO getWallet contractsPath parent timestamp utxoSet acs ema contractWallets block
    
    match result with 
    | Ok _ -> ()
    | Error error -> failwith error
    