module Blockchain.ExtendedBlockHeader

open Blockchain
open Consensus
open Consensus.Block
open Consensus.Types
open Infrastructure

// TODO: serialize and deserialize for persistence

type BlockStatus = 
    | Orphan 
    | Connected
    | Invalid

[<NoComparisonAttribute>]
type T = {
    hash:Hash.Hash
    header: BlockHeader
    status: BlockStatus    
    chainWork: bigint option
    txMerkleRoot:Hash.Hash;
    witnessMerkleRoot:Hash.Hash;
    activeContractSetMerkleRoot:Hash.Hash;
    commitments: Hash.Hash list
}

let status extendedHeader = extendedHeader.status

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
       txMerkleRoot = Hash.zero
       witnessMerkleRoot = Hash.zero
       activeContractSetMerkleRoot = Hash.zero
       commitments = []   
   } 

let createOrphan blockHash (block:Block) = 
    {
        hash = blockHash
        header = block.header
        status = Orphan        
        chainWork = None
        txMerkleRoot = block.txMerkleRoot
        witnessMerkleRoot = block.witnessMerkleRoot
        activeContractSetMerkleRoot = block.activeContractSetMerkleRoot
        commitments = block.commitments
    }
   
let createGenesis blockHash (block:Block) = 
    let chainWork = getChainWork 0I block.header 
    {     
        hash = blockHash
        header = block.header
        status = Connected            
        chainWork = Some chainWork 
        txMerkleRoot = block.txMerkleRoot
        witnessMerkleRoot = block.witnessMerkleRoot
        activeContractSetMerkleRoot = block.activeContractSetMerkleRoot  
        commitments = block.commitments
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
                txMerkleRoot = block.txMerkleRoot
                witnessMerkleRoot = block.witnessMerkleRoot
                activeContractSetMerkleRoot = block.activeContractSetMerkleRoot
                commitments = block.commitments   
            }                     

let unorphan extendedHeader chainWork = 
    { extendedHeader with status = Connected;chainWork= Some chainWork }        
    
let setChainWork chainWork extendedHeader = {extendedHeader with chainWork=Some chainWork}

let invalid extendedHeader = {extendedHeader with status=Invalid}

let isValid extendedHeader = 
    match extendedHeader.status with 
    | Invalid -> false
    | _ -> true
    