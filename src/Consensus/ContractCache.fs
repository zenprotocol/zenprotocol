module Consensus.ContractCache

open Consensus.Types
open Consensus.Contract

type T = Map<ContractId, ContractMainFn * ContractCostFn>

let empty = Map.empty

let tryFind = Map.tryFind

let add contract = Map.add contract.contractId (contract.mainFn, contract.costFn)