module Consensus.Tests.BlockHeaderTests

open Consensus
open Consensus
open Consensus.Types
open Consensus.ChainParameters
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]  
let ``validating block header with invalid proof of work``(header: BlockHeader) =
    // changing the difficulty to very high one
    let header = {header with difficulty = 402690497ul}
    
    let expected :Result<BlockHeader, string> = Error "proof of work failed"
    
    BlockHeader.validate Chain.Test header = expected

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]  
let ``validating block header with correct proof of work``(header:BlockHeader) = 
    // changing the difficulty to easiest one
    let header = {header with difficulty = 0x20fffffful }
  
    BlockHeader.validate Chain.Test header = Ok header
    
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]  
let ``seralizing and deserialing yield same header``(header:BlockHeader) = 
    let roundTrip = BlockHeader.serialize >> BlockHeader.deserialize
    
    roundTrip header = Some header   

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]      
let ``header with wrong size doesn't deserialize``(header) =
    Array.length header <> BlockHeader.Size ==> (BlockHeader.deserialize header = None)
    
 
        
    
    
            