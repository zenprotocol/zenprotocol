module Consensus.UtxoSet

// UtxoSet is only saving the delta from the persisted utxoset
// So whenever something is not in the set we try to fetch it from db

open Consensus.Types
open Infrastructure

type OutputStatus =
    | NoOutput
    | Spent of Output       // Used for weight lookups
    | Unspent of Output

type T = Map<Outpoint, OutputStatus>

/// Means that the utxoset is equal to the persisted utxoset
let asDatabase = Map.empty

let get getUTXO set outpoint =
    match Map.tryFind outpoint set with
        | Some x-> x
        | None -> getUTXO outpoint

let getOutput getUTXO set outpoint = 
    get getUTXO set outpoint
    |> function
    | Unspent output -> output
    | NoOutput 
    | Spent _ ->
        failwith "Expected output to be unspent"

let handleTransaction getUTXO txHash tx set =
    let folder state input =
        Map.add input (Spent <| getOutput getUTXO state input) state

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

let private fromOutputStatus status =
    match status with
    | Spent output
    | Unspent output -> Some output
    | NoOutput -> None

let tryGetOutput getUTXO utxoSet outpoint =
    let status = get getUTXO utxoSet outpoint
    fromOutputStatus status

let tryGetOutputs getUTXO utxoSet inputs =
    inputs
    |> List.choose (function | Outpoint outpoint -> Some outpoint | Mint _ -> None)
    |> Option.traverseM (tryGetOutput getUTXO utxoSet)

// Use an applicative traversable to collate the errors
let getUtxosResult getUTXO inputs utxoSet =
    inputs
    |> List.choose (function | Outpoint outpoint -> Some outpoint | Mint _ -> None)
    |> Result.traverseResultA
        (fun outpoint ->
            match get getUTXO utxoSet outpoint with
            | Unspent output -> Ok output
            | err -> Error [err])

// Currently only used for tests
let getUtxos getUTXO outpoints utxoSet =
    let inputs = List.map Outpoint outpoints
    getUtxosResult getUTXO inputs utxoSet |> Option.ofResult

let undoBlock getUTXO block utxoSet =

    // remove all outputs
    List.foldBack (fun ex utxoSet ->        
        let utxoSet =
            ex.tx.inputs
            |> List.choose (function | Outpoint outpoint -> Some outpoint | Mint _ -> None)
            |> List.fold (fun utxoSet input ->
                match get getUTXO utxoSet input with
                | Spent output ->
                    Map.add input (Unspent output) utxoSet
                | NoOutput
                | Unspent _ -> failwith "Expected output to be spent") utxoSet

        ex.tx.outputs
        |> List.mapi (fun i output -> i,output )
        |> List.fold (fun utxoSet (i,output) ->
            if Transaction.isOutputSpendable output then
                let outpoint = {txHash=ex.txHash; index=uint32 i}
                Map.add outpoint NoOutput utxoSet
            else
                utxoSet) utxoSet) block.transactions utxoSet
