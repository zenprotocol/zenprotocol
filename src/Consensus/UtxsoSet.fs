module Consensus.UtxoSet

open Consensus.Types

type T = Map<Outpoint, Output> 

let create () = 
    Map.empty
    
let handleTransaction txHash tx utxos =
           
    let utxos = List.fold (fun state input -> Map.remove input state) utxos tx.inputs
    
    let outputsWithIndex = List.mapi (fun i output -> (uint32 i,output)) tx.outputs
     
    List.fold (fun state (index,output) -> 
        let outpoint = {txHash=txHash;index=index;}
        Map.add outpoint output state) utxos outputsWithIndex

let getUtxos utxos outpoints =
    List.foldBack (fun outpoint state -> 
        match state with
            | None -> None
            | Some list -> 
                match Map.tryFind outpoint utxos with
                    | None -> None
                    | Some output -> Some (output :: list)) 
        outpoints (Some [])
