module Consensus.ActiveContractSet

open Consensus
open Consensus.Hash
open Consensus.Types

type ContractKey =
    { contractId: ContractId
      expiry: uint32
      code: string }

type Change =
    | Updated of ContractKey
    | Expired

type Changes = Map<ContractId, Change>
type UndoData = (ContractId * ContractKey option) list
type T = SparseMerkleTree.T<Contract.T> * Changes

let cwt = "zenprotocol-activecontractset"B

// Initialization
let empty: T =
    let merkleTree =
        SparseMerkleTree.create cwt (fun (contract: Contract.T) ->
            let bytes = System.BitConverter.GetBytes contract.expiry

            if System.BitConverter.IsLittleEndian then
                Array.rev bytes
            else
                bytes)

    merkleTree, Map.empty

let load (contracts: Contract.T list) : T =
    let keyValuePairs = 
        contracts
        |> List.map (fun contract -> ContractId.contractHash contract.contractId, contract)
        |> List.toArray

    let tree = SparseMerkleTree.addMultiple (fst empty) keyValuePairs

    tree, snd empty

let private key (contract: Contract.T) : ContractKey =
    { contractId = contract.contractId
      code = contract.code
      expiry = contract.expiry }

let private remove contractsToRemove (acs: T) : T =
    let contractsToRemove =
        Seq.choose
            (fun cHash -> Map.tryFind cHash (fst acs).data |> Option.map (fun value -> cHash, value))
            contractsToRemove

    let changes =
        Seq.fold
            (fun changes (_, (value: Contract.T)) -> Map.add value.contractId Expired changes)
            (snd acs)
            contractsToRemove

    let merkleTree =
        contractsToRemove
        |> Seq.map (fun (key, _) -> key, SparseMerkleTree.Empty)
        |> Seq.toArray
        |> SparseMerkleTree.updateMultiple (fst acs)

    merkleTree, changes

// Main functionalities
let add contractId value (acs: T) : T =
    let change = Updated(key value)

    let merkleTree =
        SparseMerkleTree.add (ContractId.contractHash contractId) value (fst acs)

    let changes = Map.add contractId change (snd acs)
    merkleTree, changes

let expireContracts blockNumber (acs: T) : T =
    let contractsToRemove = 
        (fst acs).data
        |> Map.filter (fun _ value -> value.expiry = blockNumber)
        |> Map.toSeq
        |> Seq.map fst

    remove contractsToRemove acs

// Utilities

let tryFind (ContractId(version, cHash)) (acs: T) =
    SparseMerkleTree.tryFind cHash (fst acs)
    |> Option.filter (fun contract -> contract.version = version)

let containsContract contractId acs = Option.isSome (tryFind contractId acs)

let getContracts (acs: T) =
    (fst acs).data |> Map.toSeq |> Seq.map snd

let root (acs: T) = SparseMerkleTree.root (fst acs)

let getUndoData after before : UndoData =
    let getKeys (tree: SparseMerkleTree.T<Contract.T>) = 
        tree.data
        |> Map.toSeq 
        |> Seq.map fst 
        |> Set.ofSeq

    let unionKeys = Set.union (getKeys (fst after)) (getKeys (fst before))

    let computeUndoData undoData cHash =
        let beforeData = Map.tryFind cHash ((fst before): SparseMerkleTree.T<Contract.T>).data
        let afterData = Map.tryFind cHash ((fst after): SparseMerkleTree.T<Contract.T>).data
        match beforeData, afterData with
        | Some b, None -> (b.contractId, key b |> Some) :: undoData
        | None, Some a -> (a.contractId, None) :: undoData
        | Some b, Some a when b.expiry <> a.expiry -> (b.contractId, key b |> Some) :: undoData
        | _ -> undoData

    Set.fold computeUndoData List.empty unionKeys


let undoBlock loadContract undoData acs : T =
    let toRemove =
        List.filter (snd >> Option.isNone) undoData
        |> List.map (fst >> ContractId.contractHash)

    let acs = remove toRemove acs

    List.fold
        (fun acs (contractId, contract) ->
            match contract with
            | Some contract ->
                let contract = loadContract contract.expiry contract.code contract.contractId
                add contractId contract acs
            | None -> acs)
        acs
        undoData

let clearChanges (acs: T) : T = fst acs, Map.empty