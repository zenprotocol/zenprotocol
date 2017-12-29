module Consensus.MerkleTree
open Consensus.Hash

let private findSplitIndex length = 
    Seq.initInfinite (fun i -> pown 2 i)
    |> Seq.find (fun i -> i * 2 >= length)

let rec computeRoot (xs:Hash.Hash list) =
    match xs with
    | head :: [] -> head
    | xs ->
        let splitIndex = findSplitIndex (List.length xs)
        
        let left,right = List.splitAt splitIndex xs
        let leftHash = computeRoot left
        let rightHash = computeRoot right
        
        Hash.computeMultiple (seq {yield (Hash.bytes leftHash); yield (Hash.bytes rightHash)})

let rec createAuditPath xs hashIndex =
    match xs with
    | _ :: [] -> []
    | xs ->
        let splitIndex = findSplitIndex (List.length xs)
        let left,right = List.splitAt splitIndex xs
        
        match hashIndex < splitIndex with
        | true ->
            let path = createAuditPath left hashIndex
            let rightHash = computeRoot right
            
            rightHash :: path
        | false ->
            let path = createAuditPath right (hashIndex - splitIndex)
            let leftHash = computeRoot left
            
            leftHash :: path
            
let rec verify auditPath hashIndex hash = 
    match auditPath with
    | [] -> hash
    | head :: tail ->
        let length = pown 2 (List.length auditPath)
        let splitIndex = findSplitIndex length
        
        let leftHash,rightHash = 
            match hashIndex < splitIndex with
            | true -> 
                let leftHash = verify tail hashIndex hash
                leftHash, head
            | false ->
                let rightHash = verify tail (hashIndex - splitIndex) hash
                head,rightHash
                
        Hash.computeMultiple (seq {yield (Hash.bytes leftHash); yield (Hash.bytes rightHash)})