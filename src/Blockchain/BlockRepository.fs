module Blockchain.BlockRepository

open Infrastructure
open Blockchain
open Consensus
open Consensus
open Consensus.Types
open DataAccess
open MBrace.FsPickler.Combinators
open Blockchain.DatabaseContext
                             
let contains session blockHash = 
    Collection.containsKey session.context.blocks session.session blockHash                              
                             
let tryGetHeader session hash = 
    Collection.tryGet session.context.blocks session.session hash

let getHeader session hash = 
    Collection.get session.context.blocks session.session hash        
        
let saveHeader session (block:ExtendedBlockHeader.T) =
    // TODO: save transactions
    Collection.put session.context.blocks session.session block.hash block

let getFullBlock session (block:ExtendedBlockHeader.T) =      
    let transactions = 
        Collection.get session.context.blockTransactions session.session block.hash
        |> Seq.map (Collection.get session.context.transactions session.session)
        |> Seq.toList
        
    {
        header=block.header
        txMerkleRoot=block.txMerkleRoot
        witnessMerkleRoot=block.witnessMerkleRoot
        activeContractSetMerkleRoot=block.activeContractSetMerkleRoot
        commitments=block.commitments
        transactions=transactions
    }   

let saveFullBlock session blockHash (block:Block) = 
    let transactions = 
        block.transactions
        |> List.map Transaction.hash
        |> List.zip block.transactions
    
    // Save all transactions
    List.iter (fun (tx,txHash) -> Collection.put session.context.transactions session.session txHash tx) transactions 
    
    List.map snd transactions 
    |> List.toSeq
    |> Collection.put session.context.blockTransactions session.session blockHash

let saveBlockState session blockHash (utxoSet:UtxoSet.T) (acs:ActiveContractSet.T) ema = 
    let blockState = 
        {
            ema = ema
            utxoSet = utxoSet
            activeContractSet = ActiveContractSet.getContractHashes acs
        }         
        
    Collection.put session.context.blockState session.session blockHash blockState        

let getBlockState session blockHash =        
    let blockState = Collection.get session.context.blockState session.session blockHash     
    
    let acs =  
        blockState.activeContractSet
        |> Seq.map (fun cHash -> cHash,Contract.load session.context.contractPath cHash)
        |> Seq.toArray
        |> SparseMerkleTree.addMultiple ActiveContractSet.empty 
        
    blockState.utxoSet,acs,blockState.ema                    
    
let getBlockChildren session (block:ExtendedBlockHeader.T) = 
    Index.getAll session.context.blockChildrenIndex session.session block.hash             
    
let tryGetTip session = 
    match SingleValue.tryGet session.context.tip session.session with
    | Some blockHash -> 
        let header = getHeader session blockHash  
        let utxoSet,acs,ema = getBlockState session blockHash
        
        Some (header,utxoSet,acs,ema)
    | None -> None                  
    
let updateTip session blockHash = 
    SingleValue.put session.context.tip session.session blockHash     
        