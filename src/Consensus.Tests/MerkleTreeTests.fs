module Consensus.Tests.MerkleTreeTests

open Consensus
open Consensus.Types
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

[< Property(Arbitrary=[| typeof<ConsensusGenerator> |]) >]
let ``different transactions yield different merkle root`` (NonEmptyTransactions txs1) (NonEmptyTransactions txs2) =
    (txs1 <> txs2) ==> lazy(
        let txs1 = List.map Transaction.hash txs1
        let txs2 = List.map Transaction.hash txs2
        
        MerkleTree.computeRoot txs1 <> MerkleTree.computeRoot txs2)

[<Property(StartSize=1000,EndSize=10000,MaxTest=1000, Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``can create audit path and verify with root only`` (NonEmptyTransactions txs) (index:uint32) =
    let index = (int index) % (List.length txs)
    let txs = List.map Transaction.hash txs
    let tx = txs.[index]         
    
    let root = MerkleTree.computeRoot txs
    let auditPath = MerkleTree.createAuditPath txs index
    
    MerkleTree.verify root auditPath index tx 
        
        
       