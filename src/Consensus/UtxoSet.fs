module Consensus.UtxoSet

// UtxoSet is only saving the delta from the persisted utxoset
// So whenever something is not in the set we try to fetch it from db

open Consensus.Types

type OutputStatus =
    | Removed
    | Spent
    | Unspent of Output

type T = Map<Outpoint, OutputStatus>
    
let empty = Map.empty

let handleTransaction tryGetOutput txHash tx set =
    let folder state input =
        match Map.tryFind input state with
        | Some (Unspent _) -> Map.add input Spent state
        | Some Removed
        | Some Spent -> failwith "Expected output to be unspent"
        | None -> 
            // Doesn't exist in memory, trying disk
            match tryGetOutput input with
            | Some (Unspent _) -> Map.add input Spent state
            | _ -> failwith "Expected output to be unspent"
    
    let outputsWithIndex = List.mapi (fun i output -> (uint32 i,output)) tx.outputs

    let set = 
        let set = List.fold folder set tx.inputs
        
        List.fold (fun state (index,output) ->
            let outpoint = {txHash=txHash;index=index;}
            Map.add outpoint (Unspent output) state) set outputsWithIndex
        
    set

let isSomeSpent tryGetOutput outpoints set =
    List.fold (fun state outpoint ->
        match state with
        | true -> true
        | false ->
            match Map.tryFind outpoint set with
            | Some Spent -> true
            | Some Removed
            | Some (Unspent _) -> false
            | None ->
                // Doesn't exist in memory, trying disk
                match tryGetOutput outpoint with
                | Some Spent -> true
                | _ -> false
            ) false outpoints

let getUtxos tryGetOutput outpoints set =    
    List.foldBack (fun outpoint state ->
        match state with
            | None -> None
            | Some list ->
                match Map.tryFind outpoint set with
                | Some Removed                 
                | Some Spent ->
                    None
                | Some (Unspent output) -> Some (output :: list)
                | None ->
                    // Doesn't exist in memory, trying disk
                    match tryGetOutput outpoint with
                    | None
                    | Some Removed
                    | Some Spent ->
                        None
                    | Some (Unspent output) -> Some (output :: list))  
        outpoints (Some [])

let undoBlock getOutput tryGetOutput block set =

    // unmark any spent output
    let utxoSet  =
        List.map (fun tx -> tx.inputs) block.transactions
        |> List.concat
        |> List.fold (fun set input ->
            match Map.tryFind input set with
            | Some Spent -> 
                let output = getOutput input
                Map.add input (Unspent output) set
            | Some Removed
            | Some (Unspent _) -> failwith "Expected output to be spent"
            | None ->
                // Doesn't exist in memory, trying disk
                match tryGetOutput input with
                | Some Spent ->
                    let output = getOutput input
                    Map.add input (Unspent output) set
                | _ -> failwith "Expected output to be spent"
            ) set

    // remove all outputs
    List.fold (fun utxoSet tx ->
        let txHash = Transaction.hash tx

        tx.outputs
        |> List.mapi (fun i _ -> i)
        |> List.fold (fun utxoSet i ->
            let outpoint = {txHash=txHash; index=uint32 i}
                                    
            Map.add outpoint Removed utxoSet    
            ) utxoSet) utxoSet block.transactions
        