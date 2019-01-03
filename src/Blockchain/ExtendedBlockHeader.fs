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
    | MainChain
    | Invalid

[<NoComparisonAttribute>]
type T = {
    hash:Hash.Hash
    header: BlockHeader
    status: BlockStatus
    chainWork: bigint option
    txMerkleRoot: Hash.Hash
    witnessMerkleRoot: Hash.Hash
    activeContractSetMerkleRoot: Hash.Hash
    cgpCommitment: Hash.Hash option
    commitments: Hash.Hash list
}

let status extendedHeader = extendedHeader.status

let chainWork block =
     match block.chainWork with
     | Some c -> c
     | None -> failwithf "block #%d doesn't have chainWork" block.header.blockNumber

let empty =
   {
       hash = Hash.zero
       header = Block.genesisParent
       status = Orphan
       chainWork = None
       txMerkleRoot = Hash.zero
       witnessMerkleRoot = Hash.zero
       activeContractSetMerkleRoot = Hash.zero
       cgpCommitment = None
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
        cgpCommitment = block.cgpCommitment
        commitments = block.commitments
    }

let createGenesis blockHash (block:Block) =
    let chainWork = getChainWork 0I block.header

    {
        hash = blockHash
        header = block.header
        status = MainChain
        chainWork = Some chainWork
        txMerkleRoot = block.txMerkleRoot
        witnessMerkleRoot = block.witnessMerkleRoot
        activeContractSetMerkleRoot = block.activeContractSetMerkleRoot
        cgpCommitment = block.cgpCommitment
        commitments = block.commitments
    }

let private create status prevBlock blockHash (block:Block) =
    match prevBlock.chainWork with
    | None -> failwith "prevBlock doesn't have chainWork"
    | Some prevChainWork ->
        let chainWork = getChainWork prevChainWork block.header

        {
            hash = blockHash
            header = block.header
            status = status
            chainWork = Some chainWork
            txMerkleRoot = block.txMerkleRoot
            witnessMerkleRoot = block.witnessMerkleRoot
            activeContractSetMerkleRoot = block.activeContractSetMerkleRoot
            cgpCommitment = block.cgpCommitment
            commitments = block.commitments
        }

let createConnected  = create Connected

let createMain = create MainChain

let markAsMain extendedHeader = {extendedHeader with status = MainChain}

let unmarkAsMain extendedHeader = {extendedHeader with status = Connected}

let unorphan extendedHeader chainWork =
    { extendedHeader with status = Connected;chainWork= Some chainWork }

let setChainWork chainWork extendedHeader = {extendedHeader with chainWork=Some chainWork}

let invalid extendedHeader = {extendedHeader with status=Invalid}

let isValid extendedHeader = extendedHeader.status <> Invalid