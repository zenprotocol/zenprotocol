module Consensus.ActiveContractSet

open Consensus.Types

type T = SparseMerkleTree.T<Contract.T>

let cwt = "zenprotocol-activecontractset"B

// TODO: use the number of blocks left for the contract
let set = Hash.compute "zenprotocol-activecontract"B |> Hash.bytes

let empty:T = SparseMerkleTree.create cwt (fun _ -> set)

let add : _ -> _ -> T -> T = SparseMerkleTree.add<Contract.T>

let tryFind : _ -> T -> _ = SparseMerkleTree.tryFind<Contract.T>

let containsContract : _ -> T -> _ = SparseMerkleTree.containsKey<Contract.T>
    
let getContractHashes (acs:T) =
    acs.data |> Map.toSeq |> Seq.map fst