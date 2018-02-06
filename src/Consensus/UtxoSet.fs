module Consensus.UtxoSet

// UtxoSet is only saving the delta from the persisted utxoset
// So whenever something is not in the set we try to fetch it from db

open Consensus.Types
open Infrastructure

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

// Use an applicative traversable to collate the errors
let getUtxosResult getUTXO outpoints utxoSet =
    Result.traverseResultA
        (fun outpoint ->
            match get getUTXO outpoint utxoSet with
            | Unspent output -> Ok output
            | Spent -> Error [Spent]
            | NoOutput -> Error [NoOutput])
        outpoints

// Currently only used for tests
let getUtxos getUTXO outpoints utxoSet =
    getUtxosResult getUTXO outpoints utxoSet |> Option.ofResult

let undoBlock getOutput getUTXO block utxoSet =

    // remove all outputs
    List.foldBack (fun tx utxoSet ->
        let txHash = Transaction.hash tx

        let utxoSet = 
            List.fold (fun utxoSet input ->
                match get getUTXO input utxoSet with
                | Spent -> 
                    let output = getOutput input
                    Map.add input (Unspent output) utxoSet
                | NoOutput
                | Unspent _ -> failwith "Expected output to be spent") utxoSet tx.inputs
        
        List.mapi (fun i _ -> i) tx.outputs
        |> List.fold (fun utxoSet i ->
            let outpoint = {txHash=txHash; index=uint32 i}
                                    
            Map.add outpoint NoOutput utxoSet    
            ) utxoSet) block.transactions utxoSet
        