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
    match Map.tryFind outpoint set with
        | Some x-> x
        | None -> getUTXO outpoint

let handleTransaction getUTXO txHash tx set =
    let folder state input =
        match get getUTXO input state with
        | Unspent _ -> Map.add input Spent state
        | NoOutput
        | Spent -> failwith "Expected output to be unspent"

    let outputsWithIndex = List.mapi (fun i output -> (uint32 i,output)) tx.outputs

    let set =
        let set =
            tx.inputs
            |> List.choose (function | Outpoint outpoint -> Some outpoint | _ -> None)
            |> List.fold folder set

        List.fold (fun state (index,output) ->
            if Transaction.isOutputSpendable output then
                let outpoint = {txHash=txHash;index=index;}
                Map.add outpoint (Unspent output) state
            else
                state
            ) set outputsWithIndex

    set

// Use an applicative traversable to collate the errors
let getUtxosResult getUTXO inputs utxoSet =
    inputs
    |> List.choose (function | Outpoint outpoint -> Some outpoint | Mint _ -> None)
    |> Result.traverseResultA
        (fun outpoint ->
            match get getUTXO outpoint utxoSet with
            | Unspent output -> Ok output
            | Spent -> Error [Spent]
            | NoOutput -> Error [NoOutput])

// Currently only used for tests
let getUtxos getUTXO outpoints utxoSet =
    let inputs = List.map Outpoint outpoints
    getUtxosResult getUTXO inputs utxoSet |> Option.ofResult

let undoBlock getOutput getUTXO block utxoSet =

    // remove all outputs
    List.foldBack (fun tx utxoSet ->
        let txHash = Transaction.hash tx

        let utxoSet =
            tx.inputs
            |> List.choose (function | Outpoint outpoint -> Some outpoint | Mint _ -> None)
            |> List.fold (fun utxoSet input ->
                match get getUTXO input utxoSet with
                | Spent ->
                    let output = getOutput input
                    Map.add input (Unspent output) utxoSet
                | NoOutput
                | Unspent _ -> failwith "Expected output to be spent") utxoSet

        tx.outputs
        |> List.mapi (fun i output -> i,output )
        |> List.fold (fun utxoSet (i,output) ->
            if Transaction.isOutputSpendable output then
                let outpoint = {txHash=txHash; index=uint32 i}
                Map.add outpoint NoOutput utxoSet
            else
                utxoSet) utxoSet) block.transactions utxoSet
