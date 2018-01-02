module Consensus.Tests.DifficultyTests

open Consensus
open Consensus.Types
open Consensus.Tests
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``uncompressed is smaller or equal original``(original:Hash.Hash) = 
    let compressed = Difficulty.compress original    
    let uncompressed = Difficulty.uncompress compressed
    
    original >= uncompressed

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``uncompressed value of two different hashes keep the comparison result``(h1:Hash.Hash) (h2:Hash.Hash) =
    (h1 <> h2) ==>
        let c1 = Difficulty.compress h1
        let u1 = Difficulty.uncompress c1
        let c2 = Difficulty.compress h2 
        let u2 = Difficulty.uncompress c2
         
        (u1 >= u2 && h1 > h2) || (u2 >= u1 && h2 > h1) 

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]       
let ``3bytes hash equal same value after compressing``(ThreeBytesHash hash) = 
    let uncompressed = 
        Difficulty.compress hash
        |> Difficulty.uncompress
                
    uncompressed = hash
                            
[<Property(MaxTest=1000, Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``uncompressed is larger when third MSB is lower``(LeadingZerosHash original) =
    let compressed = Difficulty.compress original    
    let uncompressed = Difficulty.uncompress compressed
    
    let bytes = Hash.bytes original     
    
    let index = 
        bytes
        |> Array.mapi (fun i b -> i,b)
        |> Array.fold (fun (counter, msb) (i,b) ->
            match counter with
            | 0 when b = 0uy -> (counter, msb)
            | 0 when b <> 0uy -> (counter+1, msb)
            | 1 -> (counter+1, msb)
            | 2 -> (counter+1, i)
            | _ -> (counter, msb)) (0, -1)
        |> snd
                
    bytes.[index] <> 0uy ==> (                                 
        bytes.[index] <- bytes.[index] - 1uy
        let hash = Hash.Hash bytes
        
        hash < uncompressed
    )