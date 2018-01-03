module Blockchain.ActiveContractSet

open Consensus

type T = Map<Hash.Hash, Contract.T>

let create () = Map.empty

let containsContract = Map.containsKey 

let add = Map.add

let tryFind = Map.tryFind