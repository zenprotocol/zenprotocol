module Consensus.ActiveContractSet

type T = SparseMerkleTree.T<Contract.T>

let private cwt = "zenprotocol-activecontractset"B

// TODO: use the number of blocks left for the contract
let private set = Hash.compute "zenprotocol-activecontract"B |> Hash.bytes

let empty:T = SparseMerkleTree.create cwt (fun _ -> set)

let add = SparseMerkleTree.add

let tryFind = SparseMerkleTree.tryFind  

let containsContract = SparseMerkleTree.containsKey

    