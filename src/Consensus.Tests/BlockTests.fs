module Consensus.Tests.BlockTests

open Consensus
open Consensus.ChainParameters
open Consensus.Types
open Consensus.Tests
open Wallet
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``block with empty transactions failed validation``(header) =
    
    let block = {header=header;transactions=[]} 

    Block.validate Chain.Test block = Error "transactions is empty"
  
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``block with invalid header failed validation``(header:BlockHeader) (NonEmptyTransactions transactions) =
    let header = {header with difficulty = 402690497ul}

    let block = {header=header;transactions=transactions}
        
    Block.validate Chain.Test block = Error "proof of work failed"
    
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``block with one invalid transaction fail validation``(header) (NonEmptyTransactions transactions) (NonNegativeInt index) =
    let index = index % (List.length transactions) 
    let header = {header with difficulty = 0x20fffffful }
    
    // Making TX invalid by removing inputs
    let invalidTx = {transactions.[index] with inputs = []}
    let transactions = List.mapi (fun i tx -> if i = index then invalidTx else tx) transactions
             
    let block = {header=header;transactions=transactions}
    
    let expected = Error (sprintf "transaction %A failed validation due to General \"inputs empty\"" (Transaction.hash invalidTx))

    Block.validate Chain.Test block = expected
    
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``block with valid transactions pass validation``(header) (NonEmptyTransactions transactions) =
    let header = {header with difficulty = 0x20fffffful }         
    let block = {header=header;transactions=transactions}                   
        
    Block.validate Chain.Test block = Ok block
    
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]        
let ``connecting block failed when block number is not successive``(parent:BlockHeader) (block:Block) =
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.create ()
    let ema = EMA.create Chain.Test
 
    parent.blockNumber + 1ul <> block.header.blockNumber ==> (Block.connect parent acs utxoSet ema block = Error "blockNumber mismatch")  

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``connecting block should fail when commitments are wrong``(parent:BlockHeader) (NonEmptyTransactions transactions) = 
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.create ()
    let ema = EMA.create Chain.Test   
    
    let header = {
        version = parent.version
        parent = BlockHeader.hash parent
        blockNumber = parent.blockNumber + 1ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0x20fffffful
        nonce = 0UL,0UL        
    }
    
    let block = {header=header;transactions=transactions}
    
    Block.connect parent acs utxoSet ema block = Error "commitments mismatch"

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]    
let ``connecting block should fail when transaction inputs are invalid``(parent:BlockHeader) (NonEmptyTransactions transactions) =
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.create ()
    let ema = EMA.create Chain.Test   
 
    let block = Block.createTemplate parent 0UL ema acs transactions
        
    Block.connect parent acs utxoSet ema block = Error "transactions failed inputs validation"

[<Test>]    
let ``can connect valid block``() = 
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let tx = 
        Account.createTransaction Chain.Test rootAccount (Account.getAddress account1 Chain.Test) Hash.zero 1UL
        |> (fun x -> match x with | Ok x -> x | _ -> failwith "failed transaction generation") 
    
    let acs = ActiveContractSet.empty
    let utxoSet = UtxoSet.create () |> UtxoSet.handleTransaction Transaction.rootTxHash Transaction.rootTx
    let ema = EMA.create Chain.Test
    
    let parent = {version=0ul; parent=Hash.zero; blockNumber=0ul;commitments=Hash.zero; timestamp=0UL;difficulty=0ul;nonce=0UL,0UL}
    let block = Block.createTemplate parent 0UL ema acs [tx]
    
    let result = Block.connect parent acs utxoSet ema block
    
    match result with 
    | Ok _ -> ()
    | Error error -> failwith error
    