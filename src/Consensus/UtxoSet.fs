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
    | Some status -> status
    | None        -> getUTXO outpoint

let getOutput getUTXO set outpoint = 
    match get getUTXO set outpoint with
    | Unspent output -> output
    | NoOutput 
    | Spent _ ->
        failwith "Expected output to be unspent"

let handleTransaction getUTXO txHash tx set =
    let folder state input =
        let output = getOutput getUTXO state input
        Map.add input (Spent output) state

    let processOutputs state (index, output) =
        if Transaction.isOutputSpendable output then
            let outpoint = { txHash = txHash; index = index }
            Map.add outpoint (Unspent output) state
        else
            state

    let setWithInputs = 
        tx.inputs
        |> List.choose (function | Outpoint outpoint -> Some outpoint | _ -> None)
        |> List.fold folder set

    tx.outputs
    |> List.mapi (fun i output -> (uint32 i, output))
    |> List.fold processOutputs setWithInputs

let private fromOutputStatus status =
    match status with
    | Spent output
    | Unspent output -> Some output
    | NoOutput -> None

let tryGetOutput getUTXO utxoSet outpoint =
    get getUTXO utxoSet outpoint
    |> fromOutputStatus

let extractOutpoints inputs =
    inputs |> List.choose (function | Outpoint outpoint -> Some outpoint | _ -> None)

let tryGetOutputs getUTXO utxoSet inputs =
    extractOutpoints inputs
    |> Option.traverseM (tryGetOutput getUTXO utxoSet)

// Use an applicative traversable to collate the errors
let getUtxosResult getUTXO inputs utxoSet =
    extractOutpoints inputs
    |> Result.traverseResultA (fun outpoint ->
        match get getUTXO utxoSet outpoint with
        | Unspent output -> Ok output
        | err            -> Error [err])

// Currently only used for tests
let getUtxos getUTXO outpoints utxoSet =
    let inputs = List.map Outpoint outpoints
    getUtxosResult getUTXO inputs utxoSet |> Option.ofResult

let private revertInputState getUTXO utxoSet input =
    match get getUTXO utxoSet input with
    | Spent output -> Map.add input (Unspent output) utxoSet
    | NoOutput | Unspent _ -> failwith "Expected output to be spent"

let private processOutput txHash utxoSet (index, output) =
    if Transaction.isOutputSpendable output then
        let outpoint = { txHash = txHash; index = uint32 index }
        Map.add outpoint NoOutput utxoSet
    else
        utxoSet

let undoBlock getUTXO block utxoSet =

    // Process a single transaction to remove all its outputs
    let processTransaction txExtended utxoSet =        
        // Revert inputs to their unspent state
        let inputs = txExtended.tx.inputs |> List.choose (function | Outpoint outpoint -> Some outpoint | _ -> None)
        let utxoSetAfterRevertingInputs = List.fold (revertInputState getUTXO) utxoSet inputs
        
        // Mark outputs as not part of UTXO set if they are spendable
        let outputs = txExtended.tx.outputs |> List.mapi (fun i output -> i, output)
        List.fold (processOutput txExtended.txHash) utxoSetAfterRevertingInputs outputs

    // Process all transactions in the block
    List.foldBack processTransaction block.transactions utxoSet
