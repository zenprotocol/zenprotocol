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

let undoBlock (block:Block) acs = 
    // TODO: restore any expired contracts
    // TODO: what if user send activate transaction twice? we will actually remove it from ACS
    // TODO: we should not remove but substract the expired block height, if it lower than current, remove
    
    List.choose (fun tx -> tx.contract) block.transactions
    |> List.map Contract.computeHash 
    |> List.map (fun cHash -> cHash, SparseMerkleTree.Empty)
    |> List.toArray
    |> SparseMerkleTree.updateMultiple acs 
    