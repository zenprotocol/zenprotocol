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
    let getOk (contractId,result) =
         match result with
         | Ok x -> contractId,x
         | Error error -> failwithf "cannot load contract from db due to %A" error
  
    contracts
    |> List.map (fun contractState ->
        ContractId.contractHash contractState.contractId,
        Contract.load contractPath contractState.expiry contractState.code contractState.contractId)
    |> List.map getOk
    |> List.toArray
    |> SparseMerkleTree.addMultiple ActiveContractSet.empty
