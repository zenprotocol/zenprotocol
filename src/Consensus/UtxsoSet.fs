module Consensus.UtxoSet

open Consensus.Types

let getUtxos utxos outpoints =
    List.foldBack (fun outpoint state -> 
        match state with
            | None -> None
            | Some list -> 
                match Map.tryFind outpoint utxos with
                    | None -> None
                    | Some output -> Some (output :: list)) 
        outpoints (Some [])   
