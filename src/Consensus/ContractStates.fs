module Consensus.ContractStates

open Consensus
open Types
open Contract
open Zen.Types.Data
open Zen.Types.Main

type UndoData = List<ContractId * data option>

type T = Map<ContractId, data option>

let asDatabase = Map.empty

let private get getState contract states =
    match Map.tryFind contract states with
        | Some x -> x
        | None -> getState contract

let tryGetState getState states contractId =
    get getState contractId states

let tryFind contractId states =
    match Map.tryFind contractId states with
    | Some value -> value
    | None -> None

let update contractId data states =
    Map.add contractId (Some data) states
   
let undoBlock undoData states =
    List.fold (fun states (contractId, state) ->
        Map.add contractId state states) states undoData

let delete contractId states =
    Map.add contractId None states

let getUndoData getState afterState beforeState = 
    let get contractId =
        match Map.tryFind contractId beforeState with
        | Some value -> value
        | None -> getState contractId
        
    afterState
    |> Map.toSeq
    |> Seq.map (fun (contractId, _) ->
        contractId, get contractId)
    |> Seq.toList