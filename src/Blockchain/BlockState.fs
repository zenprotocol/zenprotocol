module Blockchain.BlockState

open Consensus
open Consensus.Types

type ContractState = {
    contractId: ContractId
    expiry:uint32
    code:string
}

type T =
    {
        ema:EMA.T
        activeContractSet:ContractState list
        contractStatesUndoData:ContractStates.UndoData
    }
    
let initAcs (contracts : ContractState list) contractPath =
    contracts
    |> List.map (fun contractState ->
        ContractId.contractHash contractState.contractId,
        Contract.load contractPath contractState.expiry contractState.code contractState.contractId
        |> Result.mapError failwith
        |> Infrastructure.Result.get)
    |> List.toArray
    |> SparseMerkleTree.addMultiple ActiveContractSet.empty
