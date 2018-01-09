module Consensus.Tests.MerkleTreeTests

open Consensus
open Consensus.Types
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

[<Property(StartSize=1000,EndSize=10000,MaxTest=100, Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``different sets yield different merkle root`` (UniqueHashes xs1) (UniqueHashes xs2) =
    (xs1 <> xs2) ==> lazy(                
        MerkleTree.computeRoot xs1 <> MerkleTree.computeRoot xs2)

[<Property(StartSize=1000,EndSize=10000,MaxTest=100, Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``can create audit path and verify with root only`` (UniqueHashes xs) (index:uint32) =
    let index = (int index) % (List.length xs)    
    let x = xs.[index]         
    
    let root = MerkleTree.computeRoot xs
    let auditPath = MerkleTree.createAuditPath xs index
    
    MerkleTree.verify root auditPath index x 
        
        
       