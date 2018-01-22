module Consensus.ActiveContractSet

open Consensus.Types

type T = SparseMerkleTree.T<Contract.T>

let cwt = "zenprotocol-activecontractset"B

// TODO: use the number of blocks left for the contract
let set = Hash.compute "zenprotocol-activecontract"B |> Hash.bytes

let empty:T = SparseMerkleTree.create cwt (fun _ -> set)

let add = SparseMerkleTree.add

let tryFind = SparseMerkleTree.tryFind  

let containsContract = SparseMerkleTree.containsKey
    
let getContractHashes (acs:T) =
    acs.data |> Map.toSeq |> Seq.map fst