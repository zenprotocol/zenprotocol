module Consensus.Tests.MerkleMountainRangeTests

open Consensus
open Consensus.Types
open Consensus.Tests
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

[<Property(StartSize=1000,EndSize=10000,MaxTest=100, Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``different sets yield different mrr root`` (UniqueHashes xs1) (UniqueHashes xs2) =
    (xs1 <> xs2) ==> lazy(
        let mmr1 = 
            let mmr = MerkleMountainRange.create<Hash.Hash,Hash.Hash> (fun _ -> id)
            List.fold (fun mmr tx -> MerkleMountainRange.add tx tx mmr) mmr xs1 

        let mmr2 = 
            let mmr = MerkleMountainRange.create<Hash.Hash,Hash.Hash> (fun _ -> id)
            List.fold (fun mmr tx -> MerkleMountainRange.add tx tx mmr) mmr xs2
            
        MerkleMountainRange.root mmr1 <> MerkleMountainRange.root mmr2)

[<Property(StartSize=1000,EndSize=10000,MaxTest=100, Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``mmr and merkle tree should yield same hash``(UniqueHashes xs) =     
    let mmr = 
        let mmr = MerkleMountainRange.create<Hash.Hash,Hash.Hash> (fun _ -> id)
        List.fold (fun mmr tx -> MerkleMountainRange.add tx tx mmr) mmr xs 

    let merkleRoot = MerkleTree.computeRoot xs

    merkleRoot = MerkleMountainRange.root mmr

[<Property(StartSize=1000,EndSize=10000,MaxTest=100, Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``mmr and merkle root should yield same audit path``(UniqueHashes xs) (index:uint32) =
    let index = (int index) % (List.length xs)
    let tx = xs.[index]
    let mmr = 
        let mmr = MerkleMountainRange.create<Hash.Hash,Hash.Hash> (fun _ -> id)
        List.fold (fun mmr tx -> MerkleMountainRange.add tx tx mmr) mmr xs
        
    let auditPath,_ = MerkleMountainRange.createAuditPath mmr tx
    let auditPath' = MerkleTree.createAuditPath xs index
    
    auditPath = auditPath'
                     
[<Property(StartSize=1000,EndSize=10000,MaxTest=100, Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``can create audit path and verify with root only`` (UniqueHashes xs) (index:uint32) =
    let index = (int index) % (List.length xs)
    let tx = xs.[index]
    let mmr = 
        let mmr = MerkleMountainRange.create<Hash.Hash,Hash.Hash> (fun _ -> id)
        List.fold (fun mmr tx -> MerkleMountainRange.add tx tx mmr) mmr xs                                                                                
                                        
    let root = MerkleMountainRange.root mmr
    
    let auditPath, index' = MerkleMountainRange.createAuditPath mmr tx        
    
    index' = index && MerkleMountainRange.verify mmr root auditPath index tx tx

[<Property(StartSize=1000,EndSize=10000,MaxTest=100)>]
let ``updating a record yield same root as merkle tree``(NonEmptySet set: NonEmptySet<NonEmptyString>) (index:uint32) (NonEmptyString value) =
    let index = (int index) % (Set.count set)
    
    let serialize (x:string) = Hash.compute(System.Text.Encoding.UTF8.GetBytes(x))
    
    let root =
        set 
        |> Set.toList
        |> List.mapi (fun i (NonEmptyString x) -> i, (serialize x))
        |> List.fold (fun state (i,x) -> Map.add i x state) Map.empty
        |> Map.add index (serialize value)
        |> Map.toList
        |> List.map snd
        |> MerkleTree.computeRoot           

    let root' = 
        let mmr = MerkleMountainRange.create (fun _ -> serialize)
        
        set
        |> Set.toList
        |> List.mapi (fun i (NonEmptyString x) -> i, x)                
        |> List.fold (fun mmr (i,x) -> MerkleMountainRange.add i x mmr) mmr
        |> MerkleMountainRange.update index value
        |> MerkleMountainRange.root
        
    root' = root        
        
             
 
    
            
     
     