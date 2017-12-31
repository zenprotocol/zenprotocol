module Consensus.MerkleMountainRange

type Tree = 
    | Node of hash:Hash.Hash * length:int * left:Tree*right:Tree
    | Leaf of Hash.Hash
    
type T<'k, 'v when 'k : comparison> = 
    {
        tree:Tree option;
        data:Map<'k, (int*'v)>;
        serializer: 'k -> 'v -> Hash.Hash
    }
    
let private isPerfect length = 
    let bits = 
        seq {0..31}
        |> Seq.fold (fun count n -> count + ((length >>> n) &&& 1 )) 0 
        
    bits = 1        
    
let private findSplitIndex length = 
    Seq.initInfinite (fun i -> pown 2 i)
    |> Seq.find (fun i -> i * 2 >= length)
     
let private getHash tree =
    match tree with 
    | Node (hash,_,_,_) -> hash
    | Leaf (hash) -> hash 
    
let private joinHashes left right =
    Hash.computeMultiple (seq {yield (getHash left |> Hash.bytes); yield (getHash right |> Hash.bytes)})        
     
let create<'k,'v when 'k : comparison> (serializer:'k -> 'v->Hash.Hash) = 
    {tree=None;serializer=serializer; data = Map.empty }

let isEmpty mmr =
    match mmr.tree with
    | None -> true
    | Some _ -> false

let root mmr =
    Option.get mmr.tree |> getHash

let find key mmr = Map.find key mmr.data |> snd
   
let tryFind key mmr = Map.tryFind key mmr.data |> Option.map snd
    
let add key value mmr = 
    let leaf = Leaf (mmr.serializer key value)

    let rec add' tree =
        match tree with
        | Leaf left ->
            let leftLeaf = Leaf left
            let hash = joinHashes leftLeaf leaf
            Node (hash, 2, leftLeaf, leaf)
        | Node (hash,length,left,right) ->
            match isPerfect length with
            | true ->
                let left = Node (hash,length,left,right)
                let hash = joinHashes left leaf
                
                Node (hash, length + 1, left, leaf)
            | false ->
                let right = add' right
                let hash = joinHashes left right
                
                Node (hash, length + 1, left, right)
    
    let length = Map.count mmr.data
    let data = 
        Map.add key (length,value) mmr.data
    
    let tree = 
        match mmr.tree with
        | Some tree ->
            add' tree
        | None ->
            leaf
    
    {mmr with tree=Some tree;data=data}
    
let update key value mmr = 
    let leaf = Leaf (mmr.serializer key value)

    let rec update' tree index = 
        match tree with 
        | Leaf _ -> leaf
        | Node (_,length,left,right) ->
            let splitIndex = findSplitIndex length
            
            match index < splitIndex with
            | true ->
                let left = update' left index
                let hash = joinHashes left right
                                
                Node (hash, length, left, right)
            | false ->
                let right = update' right (index - splitIndex)
                let hash = joinHashes left right
                                                
                Node (hash, length, left, right)
        
    let index,_ = Map.find key mmr.data
    
    let data = Map.add key (index, value) mmr.data
    let tree = update' (Option.get mmr.tree) index
    
    {mmr with tree=Some tree;data=data}
    
let createAuditPath mmr key =
    let rec createAuditPath' tree index = 
        match tree with 
        | Leaf _ -> []
        | Node (_,length,left,right) ->
            let splitIndex = findSplitIndex length
            
            match index < splitIndex with
            | true ->
                let path = createAuditPath' left index
                let rightHash = getHash right                                
                
                rightHash :: path
            | false ->
                let path = createAuditPath' right (index - splitIndex)
                let leftHash = getHash left                                
                
                leftHash :: path
        
    let index,_ = Map.find key mmr.data
    
    (createAuditPath' (Option.get mmr.tree) index),index
    
let verify mmr root auditPath index key value = 
    let rec verify' auditPath index =
        match auditPath with
        | [] -> mmr.serializer key value
        | head :: tail ->
            let length = max (pown 2 (List.length auditPath)) (index + 1)
            let splitIndex = findSplitIndex length
            
            let leftHash,rightHash = 
                match index < splitIndex with
                | true -> 
                    let leftHash = verify' tail index
                    leftHash, head
                | false ->
                    let rightHash = verify' tail (index - splitIndex)
                    head,rightHash
                
            Hash.computeMultiple (seq {yield (Hash.bytes leftHash); yield (Hash.bytes rightHash)})
            
    let root' = verify' auditPath index
    
    root' = root