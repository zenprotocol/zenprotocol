module Consensus.Tests.BlockHeaderTests

open Consensus
open Types
open Chain
open Serialization
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]  
let ``validating block header with invalid proof of work``(header: BlockHeader) =
    // changing the difficulty to very high one
    let header = {header with difficulty = 402690497ul}
    
    let expected :Result<BlockHeader, string> = Error "proof of work failed"
    
    Block.validateHeader Chain.localParameters header = expected

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]  
let ``validating block header with correct proof of work``(header:BlockHeader) = 
    // changing the difficulty to easiest one
    let header = {header with difficulty = 0x20fffffful }
  
    Block.validateHeader Chain.localParameters header = Ok header
    
[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]  
let ``seralizing and deserialing yield same header``(header:BlockHeader) = 
    let roundTrip = Header.serialize >> Header.deserialize
    
    roundTrip header = Some header   

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]      
let ``header with wrong size doesn't deserialize``(header) =
    Array.length header <> SerializedHeaderSize ==> (Header.deserialize header = None)