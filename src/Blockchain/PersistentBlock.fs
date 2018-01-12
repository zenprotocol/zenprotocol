module Blockchain.PersistentBlock

// TODO: save commitments

open Blockchain
open Consensus
open Consensus.Block
open Consensus.Types
open Infrastructure

type BlockStatus = 
    | Orphan 
    | Connected
    | Tip
    | Invalid

[<NoComparisonAttribute>]
type T = {
    hash:Hash.Hash
    header: BlockHeader
    status: BlockStatus    
    chainWork: bigint option
    commitments: Hash.Hash list
    transactions: Transaction list
    ema: EMA.T option
}

let status block = block.status

let chainWork block = 
     match block.chainWork with
     | Some c -> c
     | None -> failwith "block doesn't have chainWork"

let empty =     
   {
       hash = Hash.zero
       header = Block.genesisParent
       status = Orphan        
       chainWork = None
       commitments = []
       transactions = []
       ema=None
   } 

let createOrphan blockHash (block:Block) = 
    {
        hash = blockHash
        header = block.header
        status = Orphan        
        chainWork = None
        commitments = block.commitments
        transactions = block.transactions
        ema=None
    }
   
let createGenesis chain blockHash (block:Block) ema = 
    let chainWork = getChainWork 0I block.header 
    {     
        hash = blockHash
        header = block.header
        status = Tip            
        chainWork = Some chainWork   
        commitments = block.commitments
        transactions = block.transactions
        ema=Some ema
    }     
   
let createTip prevBlock blockHash (block:Block) ema =
    match prevBlock.chainWork with
    | None -> failwith "prevBlock doesn't have chainWork"
    | Some prevChainWork -> 
        let chainWork = getChainWork prevChainWork block.header
          
        {     
            hash = blockHash
            header = block.header
            status = Tip            
            chainWork = Some chainWork   
            commitments = block.commitments
            transactions = block.transactions   
            ema=Some ema           
        }
        
let createConnected prevBlock blockHash (block:Block) =
    match prevBlock.chainWork with
        | None -> failwith "prevBlock doesn't have chainWork"
        | Some prevChainWork -> 
            let chainWork = getChainWork prevChainWork block.header  
            {     
                hash = blockHash
                header = block.header
                status = Connected                
                chainWork = Some chainWork
                commitments = block.commitments   
                transactions = block.transactions
                ema=None
            }

let ema block = Option.get block.ema

let addEMA block ema = 
    {block with ema = Some ema}
     
let untipBlock block = 
    { block with status = Connected }
    
let markBlockAsTip block = 
    { block with status = Tip }                        

let unorphanBlock block chainWork = 
    { block with status = Connected;chainWork= Some chainWork }        
    
let setChainWork chainWork block = {block with chainWork=Some chainWork}

let header (block:T) = block.header

let hash block = block.hash

let invalid block = {block with status=Invalid}

let isValid block = 
    match block.status with 
    | Invalid -> false
    | _ -> true

let fetchBlock (block:T) = 
    {header=block.header;transactions=block.transactions;commitments=block.commitments}
    
    