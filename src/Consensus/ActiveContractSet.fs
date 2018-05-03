module Consensus.ActiveContractSet

open Consensus.Types

type T = SparseMerkleTree.T<Contract.T>

let cwt = "zenprotocol-activecontractset"B

let empty:T = SparseMerkleTree.create cwt (fun contract ->
    let bytes = System.BitConverter.GetBytes contract.expiry

    if System.BitConverter.IsLittleEndian then
        Array.rev bytes
    else
        bytes
 )

let add (ContractId (_,cHash)) = SparseMerkleTree.add<Contract.T> cHash

let tryFind (ContractId (version,cHash)) (acs:T) =
    SparseMerkleTree.tryFind cHash acs
    |> Option.filter (fun contract -> contract.version = version)

let containsContract contractId acs = tryFind contractId acs |> Option.isSome

let getContracts (acs:T) =
    acs.data |> Map.toSeq |> Seq.map snd

let expireContracts blockNumber (acs:T) =
    let contractsToRemove =
        Map.filter (fun _ (value:Contract.T) -> value.expiry = blockNumber) acs.data
        |> Map.toSeq
        |> Seq.map (fun (key,_) -> key, SparseMerkleTree.Empty)
        |> Seq.toArray

    SparseMerkleTree.updateMultiple acs contractsToRemove

