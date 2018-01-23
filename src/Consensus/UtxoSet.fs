module Consensus.UtxoSet

// UtxoSet is only saving the delta from the persisted utxoset
// So whenever something is not in the set we try to fetch it from db

open Consensus.Types

type OutputStatus =
    | NoOutput
    | Spent
    | Unspent of Output

type T = Map<Outpoint, OutputStatus>
    
/// Means that the utxoset is equal to the persisted utxoset    
let asDatabase = Map.empty

let get getUTXO outpoint set =
    defaultArg
        <| Map.tryFind outpoint set
        <| getUTXO outpoint     // return value if outpoint isn't in set
         
let handleTransaction getUTXO txHash tx set =
    let folder state input =
        match get getUTXO input state with
        | Unspent _ -> Map.add input Spent state
        | NoOutput
        | Spent -> failwith "Expected output to be unspent"
                
    let outputsWithIndex = List.mapi (fun i output -> (uint32 i,output)) tx.outputs

    let set = 
        let set = List.fold folder set tx.inputs
        
        List.fold (fun state (index,output) ->
            let outpoint = {txHash=txHash;index=index;}
            Map.add outpoint (Unspent output) state) set outputsWithIndex
        
    set

let isSomeSpent getUTXO outpoints set =
    List.exists (fun outpoint -> get getUTXO outpoint set = Spent) outpoints

let getUtxos getUTXO outpoints set =    
    List.foldBack (fun outpoint state ->
        match state with
            | None -> None
            | Some list ->
                match get getUTXO outpoint set with
                | NoOutput                 
                | Spent ->
                    None
                | Unspent output -> Some (output :: list))                
        outpoints (Some [])

let undoBlock getOutput getUTXO block set =

    // unmark any spent output
    let utxoSet  =
        List.map (fun tx -> tx.inputs) block.transactions
        |> List.concat
        |> List.fold (fun set input ->
            match get getUTXO input set with
            | Spent -> 
                let output = getOutput input
                Map.add input (Unspent output) set
            | NoOutput
            | Unspent _ -> failwith "Expected output to be spent"            
            ) set

    // remove all outputs
    List.fold (fun utxoSet tx ->
        let txHash = Transaction.hash tx

        tx.outputs
        |> List.mapi (fun i _ -> i)
        |> List.fold (fun utxoSet i ->
            let outpoint = {txHash=txHash; index=uint32 i}
                                    
            Map.add outpoint NoOutput utxoSet    
            ) utxoSet) utxoSet block.transactions
        