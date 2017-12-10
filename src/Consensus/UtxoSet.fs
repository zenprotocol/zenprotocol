module Consensus.UtxoSet

open Consensus.Types

type InputStatus = 
    | Spent
    | Unspent of Output

type T = Map<Outpoint, InputStatus>

let create () = 
    Map.empty
    
let handleTransaction txHash tx set =           
    let utxos = List.fold (fun state input -> Map.add input Spent state) set tx.inputs    
    let outputsWithIndex = List.mapi (fun i output -> (uint32 i,output)) tx.outputs
     
    List.fold (fun state (index,output) -> 
        let outpoint = {txHash=txHash;index=index;}
        Map.add outpoint (Unspent output) state) utxos outputsWithIndex
        
let isSomeSpent outpoints set =
    List.fold (fun state outpoint ->
        match state with
        | true -> true
        | false ->
            match Map.tryFind outpoint set with
                | Some Spent -> true
                | _ -> false) false outpoints 
     
let getUtxos outpoints set =
    List.foldBack (fun outpoint state -> 
        match state with
            | None -> None
            | Some list -> 
                match Map.tryFind outpoint set with
                    | None 
                    | Some Spent -> None
                    | Some (Unspent output) -> Some (output :: list)) 
        outpoints (Some [])
