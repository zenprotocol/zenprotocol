module Consensus.Tests.ContractIdTests

open Consensus
open Consensus.Types
open Consensus.Tests
open FsCheck
open FsCheck.NUnit
open FsUnit
open TestsInfrastructure.Constraints

[<Property(Arbitrary=[| typeof<ConsensusGenerator> |])>]
let ``to and from string yield same contractId``(contractId:ContractId) =
    let b16 = (ContractId.toString contractId)
    let contractId' = ContractId.fromString b16

    Some contractId = contractId'
