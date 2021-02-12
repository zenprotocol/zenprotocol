module Consensus.Tests.SparseMerkleTreeTests

open NUnit.Framework
open FsUnit
open Consensus
open Consensus.Tests
open System

let cwt = "hello"B
let serializer (value:int) = System.BitConverter.GetBytes value

[<Test>]
let ``verify key doest exist when tree empty``() = 
    let tree = SparseMerkleTree.create cwt serializer
    
    let key = Hash.compute "0"B
    
    let auditPath = SparseMerkleTree.createAuditPath key tree
    
    SparseMerkleTree.verifyEmpty tree tree.root auditPath key |> should equal true
    SparseMerkleTree.verifyValue tree tree.root auditPath key 0 |> should equal false 

[<Test>]    
let ``add key and verify``() =
    let key = Hash.compute "0"B 
    
    let tree = 
        SparseMerkleTree.create cwt serializer
        |> SparseMerkleTree.add key 0
                
    let auditPath = SparseMerkleTree.createAuditPath key tree
    
    SparseMerkleTree.verifyValue tree tree.root auditPath key 0 |> should equal true
    SparseMerkleTree.verifyValue tree tree.root auditPath key 1 |> should equal false
    SparseMerkleTree.verifyEmpty tree tree.root auditPath key |> should equal false

[<Test>]
let ``remove key and verify``() =          
    let key = Hash.compute "0"B 
        
    let tree = 
        SparseMerkleTree.create cwt serializer
        |> SparseMerkleTree.add key 0
        |> SparseMerkleTree.remove key
                
    let auditPath = SparseMerkleTree.createAuditPath key tree
    
    SparseMerkleTree.verifyEmpty tree tree.root auditPath key |> should equal true
    SparseMerkleTree.verifyValue tree tree.root auditPath key 1 |> should equal false
    SparseMerkleTree.verifyValue tree tree.root auditPath key 0 |> should equal false

[<Test>]    
let ``creating a lot of keys``() = 
    let createKey (i:int) = (Hash.compute (BitConverter.GetBytes i)),i 
   
    let keys =  seq {1..100} |> Seq.map createKey

    let tree =
        keys
        |> Seq.map (fun (key,value) -> key, SparseMerkleTree.Value value)
        |> Seq.toArray
        |> SparseMerkleTree.updateMultiple (SparseMerkleTree.create cwt serializer)
        
    // validating all keys are present
    keys
    |> Seq.iter (fun (key,value) ->
        let auditPath = SparseMerkleTree.createAuditPath key tree
        
        SparseMerkleTree.verifyValue tree tree.root auditPath key value |> should equal true 
        SparseMerkleTree.verifyEmpty tree tree.root auditPath key |> should equal false)
    
    // validating other keys are not there                     
    seq {101..200}
    |> Seq.map (fun i -> (Hash.compute (BitConverter.GetBytes i)),i)
    |> Seq.iter (fun (key,value) ->
        let auditPath = SparseMerkleTree.createAuditPath key tree
       
        SparseMerkleTree.verifyValue tree tree.root auditPath key value |> should equal false 
        SparseMerkleTree.verifyEmpty tree tree.root auditPath key |> should equal true)
        
    // removing some keys
    let removingKeys = seq {76..100} |> Seq.map createKey        
    let leftKeys = seq {1..75} |> Seq.map createKey
        
    let tree' = 
        removingKeys
        |> Seq.map (fun (key,_) -> key, SparseMerkleTree.Empty)
        |> Seq.toArray
        |> SparseMerkleTree.updateMultiple tree
        
    leftKeys
        |> Seq.iter (fun (key,value) ->
            let auditPath = SparseMerkleTree.createAuditPath key tree'
            
            SparseMerkleTree.verifyValue tree' tree'.root auditPath key value |> should equal true 
            SparseMerkleTree.verifyEmpty tree' tree'.root auditPath key |> should equal false)                  
        
    removingKeys
    |> Seq.iter (fun (key,value) ->
        let auditPath = SparseMerkleTree.createAuditPath key tree'
       
        SparseMerkleTree.verifyValue tree' tree'.root auditPath key value |> should equal false 
        SparseMerkleTree.verifyEmpty tree' tree'.root auditPath key |> should equal true)    

[<Test>]
let ``update root with auditpath only``() = 
    let tree = SparseMerkleTree.create cwt serializer
        
    let key = Hash.compute "0"B
    
    let auditPath = SparseMerkleTree.createAuditPath key tree
    
    SparseMerkleTree.verifyEmpty tree tree.root auditPath key |> should equal true
    
    let root = SparseMerkleTree.addToRoot tree auditPath key 0
    
    SparseMerkleTree.verifyValue tree root auditPath key 0 |> should equal true
                
    let tree' = SparseMerkleTree.add key 0 tree            
            
    tree'.root |> should equal root        
            
            