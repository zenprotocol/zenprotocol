module Consensus.SparseMerkleTree

open Consensus
open System

[<Literal>]
let N = 256UL

let initialBase = Hash.bytes Hash.zero

type Data<'v> = Map<Hash.Hash, 'v>

type Value<'v> = 
    | Value of 'v
    | Empty
    
type Location = UInt64 * byte array    

[<ReferenceEquality>]
type T<'v> = {
    data: Data<'v>;
    cache: Map<Location, Hash.Hash>
    cwt: byte array; // Tree-wide constant
    defaultHashes: Hash.Hash array;
    root: Hash.Hash;
    serializer: 'v -> byte[]
}

let create cwt serializer =     
    let defaultHashes =
        seq {0UL..N}
        |> Seq.fold (fun list _ ->
            match list with
            | [] -> [Hash.compute cwt]
            | (Hash.Hash prev) :: tail ->
                let h = Hash.computeMultiple (seq {yield prev; yield prev})
                h :: (Hash.Hash prev) :: tail) []
        |> List.rev
        |> List.toArray
        
    {defaultHashes=defaultHashes; data=Map.empty;cache=Map.empty;cwt=cwt;root=defaultHashes.[int N]; serializer=serializer}

let private defaultHash tree height = 
    tree.defaultHashes.[int height]

let private setBit (value:byte array) (bit:UInt64) =
    let copy = Array.copy value
    let index = int32 bit
    copy.[index/8] <- copy.[index/8] ||| (1uy <<< (7 - index % 8))
    
    copy  
    
let private isBitSet (Hash.Hash value)  (bit:UInt64) =
    let index = int32 bit 
    (value.[index / 8] &&& (1uy <<< (7 - index % 8))) <> 0uy    

let private isLeft split (Hash.Hash key) = key < split

let private split data splitIndex = 
    // TODO: using binary search might be much more effecient       
    
    let predicate = fst >> isLeft splitIndex
    Array.partition predicate data
    
let private interiorHash (Hash.Hash left) (Hash.Hash right) (height:UInt64) _base = 
    if left = right then 
        seq {yield left;yield right;}
        |> Hash.computeMultiple
    else 
        let heightBytes = 
            if BitConverter.IsLittleEndian then
                System.BitConverter.GetBytes height |> Array.rev
            else 
                System.BitConverter.GetBytes height
        
        seq {yield left;yield right;yield heightBytes; yield _base}
        |> Hash.computeMultiple

let leafHash tree _base value  =
    match value with 
    | Value value -> 
        let value = tree.serializer value
        
        seq {yield tree.cwt; yield value; yield _base;}
        |> Hash.computeMultiple    
    | Empty -> defaultHash tree 0UL    
    
let rec private computeRootHash tree cache data (height:UInt64) _base = 
    match Map.tryFind (height,_base) cache with
    | Some value -> value
    | None ->
        match Seq.length data with
        | 0 -> defaultHash tree height
        | 1 when height = 0UL -> 
            let value = Seq.head data |> snd
            
            leafHash tree _base (Value value) 
        | _ when height = 0UL -> failwith "this should never happen (unsorted D or broken split?)"
        | _ ->
            let splitIndex = setBit _base (N - height)
            let leftData,rightData = split data splitIndex
            let left = computeRootHash tree cache leftData (height - 1UL) _base
            let right = computeRootHash tree cache rightData (height - 1UL) splitIndex
            interiorHash left right height _base
   
let private cacheNodes tree cache left right height _base splitIndex =
    let childHeight = height - 1UL
    let defaultHash = defaultHash tree childHeight
            
    let cache = 
        if defaultHash <> left && defaultHash <> right then
            cache
            |> Map.add (childHeight,_base) left 
            |> Map.add (childHeight,splitIndex) right
        else
            cache
            |> Map.remove (height, _base)
        
    (interiorHash left right height _base), cache
   
let rec private update' tree cache data keys (height:UInt64) _base =
    match height with
    | 0UL ->  (leafHash tree _base (Array.head keys |> snd)), cache 
    | _ ->
        let splitIndex = setBit _base (N - height)
        let leftData,rightData = split data splitIndex
        let leftKeys,rightKeys = split keys splitIndex
               
        match Array.isEmpty leftKeys,Array.isEmpty rightKeys with              
        | false,true -> 
            let left,cache = update' tree cache leftData leftKeys (height - 1UL) _base
            let right = computeRootHash tree cache rightData (height - 1UL) splitIndex
            
            cacheNodes tree cache left right height _base splitIndex                
        | true,false ->
            let left = computeRootHash tree cache leftData (height - 1UL) _base
            let right,cache = update' tree cache rightData rightKeys (height - 1UL) splitIndex
            
            cacheNodes tree cache left right height _base splitIndex
        | true,true -> failwith "this should never happen (unsorted D or broken split?)" 
        | false,false -> 
            let left,cache = update' tree cache leftData leftKeys (height - 1UL) _base
            let right,cache = update' tree cache rightData rightKeys (height - 1UL) splitIndex
            
            cacheNodes tree cache left right height _base splitIndex
                                                                     
let root tree = tree.root                  

let updateMultiple tree keys =
    if Array.isEmpty keys then
        tree
    else     
        let keys = Array.sortBy fst keys
        
        let data = Array.fold (fun data (key,value) ->
            match value with
            | Empty -> Map.remove key data
            | Value value -> Map.add key value data) tree.data keys
    
        let root,cache = update' tree tree.cache (Map.toArray data) keys N initialBase
        {tree with data=data;root=root;cache=cache}
        
let addMultiple tree keys =
    keys
    |> Array.map (fun (key,value) -> key,Value value) 
    |> updateMultiple tree         
                  
let add key value tree = 
    let data = Map.add key value tree.data    
    let root,cache = update' tree tree.cache (Map.toArray data) [| key, (Value value)|] N initialBase
    
    {tree with data=data;root=root;cache=cache;}  
    
let remove key tree = 
    let data = Map.remove key tree.data
    let root,cache = update' tree tree.cache (Map.toArray data) [| yield key, Empty |] N initialBase

    {tree with data=data;root=root;cache=cache}
    
let createAuditPath key tree =
    let rec createAuditPath' data height _base =
        if height = 0UL then 
            []
        else
            let splitIndex = setBit _base (N - height)     
            let leftData,rightData = split data splitIndex
                                              
            match isBitSet key (N-height) with
            | false -> 
                let auditPath = createAuditPath' leftData (height - 1UL) _base
                let right = computeRootHash tree tree.cache rightData (height - 1UL) splitIndex
                
                right :: auditPath
            | true ->
                let auditPath = createAuditPath' rightData (height - 1UL) splitIndex
                let left = computeRootHash tree tree.cache leftData (height - 1UL) _base
                
                left :: auditPath
                
    createAuditPath' (Map.toArray tree.data) N initialBase
    
let rec private verifyAuditPath tree key value auditPath height _base  = 
    if height = 0UL then
        leafHash tree _base value        
    else
        let splitIndex = setBit _base (N - height)     
                                        
        let left,right= 
            match isBitSet key (N - height) with
            | false -> 
                let right = List.head auditPath
                let tail = List.tail auditPath
                let left = verifyAuditPath tree key value tail (height - 1UL) _base
                
                left,right                                
            | true -> 
                let left = List.head auditPath
                let tail = List.tail auditPath
                let right = verifyAuditPath tree key value tail (height - 1UL) splitIndex
                
                left,right
                
        interiorHash left right height _base                              
    
let verifyValue tree root auditPath key value =              
    let root' = verifyAuditPath tree key (Value value) auditPath N initialBase
    
    root' = root
    
let verifyEmpty tree root auditPath key =
    let root' = verifyAuditPath tree key Empty auditPath N initialBase
    
    root' = root
    
let addToRoot tree auditPath key value root = 
    verifyAuditPath tree key (Value value) auditPath N initialBase
    
let removeFromRoot tree auditPath key root =    
    verifyAuditPath tree key Empty auditPath N initialBase

let tryFind key tree = 
    Map.tryFind key tree.data  
    
let containsKey key tree = Map.containsKey key tree.data          