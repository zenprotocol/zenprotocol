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
 
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with empty transactions failed validation``(header) =
    
    let block = {header=header;transactions=[];commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;} 

    Block.validate Chain.Local block = Error "transactions is empty"
  
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``block with invalid header failed validation``(header:BlockHeader) (NonEmptyTransactions transactions) =
    let header = {header with difficulty = 402690497ul}

    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}
        
    Block.validate Chain.Local block = Error "proof of work failed"
    
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``block with one invalid transaction fail validation``(header) (NonEmptyTransactions transactions) (NonNegativeInt index) =
    let index = index % (List.length transactions) 
    let header = {header with difficulty = 0x20fffffful }
    
    // Making TX invalid by removing inputs
    let invalidTx = {transactions.[index] with inputs = []}
    let transactions = List.mapi (fun i tx -> if i = index then invalidTx else tx) transactions
             
    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}
    
    let expected = Error (sprintf "transaction %A failed validation due to General \"inputs empty\"" (Transaction.hash invalidTx))

    Block.validate Chain.Local block = expected
    
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
        
    Block.validate Chain.Local block = Ok block
    
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]        
let ``connecting block failed when block number is not successive``(parent:BlockHeader) (block:Block) =

    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let ema = EMA.create Chain.Local

    parent.blockNumber + 1ul <> block.header.blockNumber 
    ==> (Block.connect Chain.Local getUTXO contractsPath parent 1UL utxoSet acs ema block = Error "blockNumber mismatch")  

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block should fail when commitments are wrong``(parent:BlockHeader) (NonEmptyTransactions transactions) = 
    
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
    
    let block = {header=header;transactions=transactions;commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}
    
    Block.connect Chain.Local getUTXO contractsPath parent (timestamp + 1UL) utxoSet acs ema block = Error "commitments mismatch"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``connecting block should fail when transaction inputs are invalid``(parent:BlockHeader) (NonEmptyTransactions transactions) =
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase
    let ema = EMA.create Chain.Local   
 
    let block = Block.createTemplate parent timestamp ema acs transactions
        
    Block.connect Chain.Local getUTXO contractsPath parent (timestamp + 1UL) utxoSet acs ema block = 
        Error "transactions failed inputs validation due to Orphan"

[<Test>]    
let ``block timestamp too early``() =
    let ema = {
        (EMA.create Chain.Local) with delayed = [timestamp-5UL .. timestamp+5UL]
    }
        
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let tx = 
        Account.createTransaction Chain.Local rootAccount account1.publicKeyHash {asset=Hash.zero;amount=1UL}
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation") 
    
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx    
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent timestamp ema acs [tx]
    
    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T) , string> = Error "block's timestamp is too early"

    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block 
    |> should equal expected
    
[<Test>]    
let ``block timestamp in the future``() =
    let ema = {
        (EMA.create Chain.Local) with delayed = [timestamp-1UL;timestamp; timestamp+1UL]
    }
        
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let tx = 
        Account.createTransaction Chain.Local rootAccount account1.publicKeyHash {asset=Hash.zero;amount=1UL}
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation") 
    
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx    
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp + Block.MaxTimeInFuture + 1UL) ema acs [tx]
    
    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T) , string> = Error "block timestamp too far in the future"
   
    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block 
    |> should equal expected

[<Test>]    
let ``block with mismatch commitments fail connecting``() = 
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let tx = 
        Account.createTransaction Chain.Local rootAccount account1.publicKeyHash { asset = Hash.zero; amount = 1UL }
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation") 
    
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Local
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0x20fffffful;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp + 1UL) ema acs [tx]
    let block = {block with commitments=[Hash.zero]}
    
    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T) , string> = Error "commitments mismatch"
    
    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block 
    |> should equal expected
    
[<Test>]    
let ``can connect valid block``() = 
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let tx = 
        Account.createTransaction Chain.Local rootAccount account1.publicKeyHash { asset = Hash.zero; amount = 1UL }
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation") 
    
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Local
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [tx]
    
    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should be ok

[<Test>]    
let ``can connect block with a contract``() = 
    let rootAccount = Account.createRoot ()    
    let tx = 
        Account.createActivateContractTransaction rootAccount SampleContract.sampleContractCode 
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation")
        
    let contract : Contract.T = 
        {
            hash=Contract.computeHash SampleContract.sampleContractCode
            fn= fun _ _ _ tx -> Ok tx
            costFn = fun _ _ _ -> Ok 0I
        }           
    
    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.hash contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Local
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [tx]
    
    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should be ok
         
[<Test>]    
let ``block with invalid contract failed connecting``() = 
    let rootAccount = Account.createRoot ()    
    let tx = 
        Account.createTransaction 
            Chain.Local rootAccount rootAccount.publicKeyHash {asset = Hash.zero; amount=1UL}
            |> function | Ok x -> x | Error error -> failwith error

    let tx = {tx with contract = Some ("ada","dasdas")}        
        
    let contract : Contract.T = 
        {
            hash=Contract.computeHash "ada"
            fn= fun _ _ _ tx -> Ok tx
            costFn = fun _ _ _ -> Ok 0I
        } 
            
    let acs = ActiveContractSet.empty |> ActiveContractSet.add contract.hash contract
    let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction getUTXO Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Local
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=timestamp;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent (timestamp+1UL) ema acs [tx]
            
    let expected : Result<(Block*UtxoSet.T*ActiveContractSet.T*EMA.T) , string> = Error "invalid contract"
    
    Block.connect Chain.Local getUTXO contractsPath parent timestamp utxoSet acs ema block
    |> should equal expected
