module Consensus.ActiveContractSet

open Consensus
open Consensus.Hash
open Consensus.Types

type ContractKey = {
    contractId: ContractId
    expiry:uint32
    code:string
}

type Change =
    | Updated of ContractKey
    | Expired

type Changes = Map<ContractId, Change>

type UndoData = (ContractId * ContractKey option) list

type T = SparseMerkleTree.T<Contract.T> * Changes

let cwt = "zenprotocol-activecontractset"B

let private key (contract:Contract.T) : ContractKey =
    {
        contractId=contract.contractId
        code=contract.code
        expiry=contract.expiry
    }

let empty:T =
    let merkleTree = SparseMerkleTree.create cwt (fun (contract:Contract.T) ->
            let bytes = System.BitConverter.GetBytes contract.expiry

            if System.BitConverter.IsLittleEndian then
                Array.rev bytes
            else
                bytes
         )

    merkleTree,Map.empty

let load (contracts:Contract.T list) =
    let (merkleTree,changes) = empty

    let merkleTree =
        List.map (fun (contract:Contract.T) -> ContractId.contractHash contract.contractId, contract) contracts
        |> List.toArray
        |> SparseMerkleTree.addMultiple merkleTree

    merkleTree,changes

let add contractId value ((merkleTree,changes):T) =
    let cHash = ContractId.contractHash contractId

    let change = Updated <| key value

    let merkleTree = SparseMerkleTree.add<Contract.T> cHash value merkleTree

    let changes = Map.add contractId change changes

    merkleTree,changes

let tryFind (ContractId (version,cHash)) ((merkleTree,_):T) =
    SparseMerkleTree.tryFind cHash merkleTree
    |> Option.filter (fun contract -> contract.version = version)

let containsContract contractId acs = tryFind contractId acs |> Option.isSome

let getContracts ((merkleTree,_):T) =
    merkleTree.data |> Map.toSeq |> Seq.map snd

let private remove (contractsToRemove: seq<Hash.Hash>) ((merkleTree,changes):T) =
    let contractsToRemove =
        Seq.choose (fun cHash ->
            Map.tryFind cHash merkleTree.data
            |> Option.map (fun value-> cHash,value)) contractsToRemove

    let changes =
        Seq.fold (fun changes (_,  (value:Contract.T)) -> Map.add value.contractId Expired changes) changes contractsToRemove

    let merkleTree =
        contractsToRemove
        |> Seq.map (fun (key,_) -> key, SparseMerkleTree.Empty)
        |> Seq.toArray
        |> SparseMerkleTree.updateMultiple merkleTree

    merkleTree, changes

let expireContracts blockNumber ((merkleTree,changes):T) =
    let contractsToRemove =
        Map.filter (fun _ (value:Contract.T) -> value.expiry = blockNumber) merkleTree.data
        |> Map.toSeq
        |> Seq.map fst

    remove contractsToRemove (merkleTree,changes)

let root ((merkleTree,_):T) = SparseMerkleTree.root merkleTree

let getUndoData ((after,_):T) ((before,_):T)  : UndoData =
    let getKeys = Map.toSeq >> Seq.map fst >> Set.ofSeq

    let afterKeys = getKeys after.data
    let beforeKeys = getKeys before.data

    let keys = Set.union afterKeys beforeKeys

    Set.fold (fun undoData cHash ->
        match Map.tryFind cHash before.data,Map.tryFind cHash after.data with
        | Some before, None -> (before.contractId, key before |> Some) :: undoData
        | None, Some after -> (after.contractId, None) :: undoData
        | Some before, Some after when before.expiry <> after.expiry -> (before.contractId, key before |> Some) :: undoData
        | _ -> undoData) List.empty keys

let undoBlock loadContract (undoData:UndoData) (acs:T) : T =
    let toRemove =
        List.filter (snd >> Option.isNone) undoData
        |> List.map (fst >> ContractId.contractHash)

    let acs = remove toRemove acs

    List.fold (fun acs (contractId,contract) ->
        match contract with
        | Some contract ->
            let contract = loadContract contract.expiry contract.code contract.contractId
            add contractId contract acs
        | None -> acs) acs undoData

let clearChanges ((merkleTree,_):T) : T = merkleTree,Map.empty