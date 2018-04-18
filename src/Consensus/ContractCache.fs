module Consensus.ContractCache

open Consensus.Hash
open Consensus.Contract

type T = Map<Hash, ContractMainFn * ContractCostFn>

let empty = Map.empty

let tryFind = Map.tryFind

let add contract = Map.add contract.hash (contract.mainFn, contract.costFn)