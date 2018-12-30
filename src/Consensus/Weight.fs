module Consensus.Weight

open System.Diagnostics.Eventing.Reader
open Consensus.Types
open TxSkeleton
open Infrastructure
open Infrastructure.Result
open Serialization

let result = new ResultBuilder<string>()

let activationFactor = 300000u
let contractExecutionFactor = 100I
let pkWitnessWeight = 100_000I
let contractSignatureWeight = 80_000I
let sizeFactor = 100u

// Returns outputs without mints. But they get added back from the inputs.
let getOutputs = UtxoSet.tryGetOutputs

let activationWeight contract =
    match contract with
    | V0 contract ->
        contract.queries * contract.rlimit / 100ul |> bigint
    | HighV _ -> 
        0I

type State =
    | Invalid of string
    | NextInput of bigint * Witness list * Input list
    | Valid of bigint

let private accumulatePkWeight total witnesses pInputs =
    match witnesses with
    | PKWitness _::witnessesTail ->
        NextInput (total + pkWitnessWeight, witnessesTail, List.tail pInputs)
    | _ -> 
        Invalid "expecting a public key witness"

let private accumulateContractWeight total witnesses pInputs =
    match witnesses with
    | [] -> Valid total
    | ContractWitness cw::witnessesTail ->
        NextInput (total
        + contractExecutionFactor * bigint cw.cost
        + (match cw.signature with
           | Some _ -> contractSignatureWeight
           | None -> 0I), witnessesTail, List.skip (int cw.inputsLength) pInputs)
    | _ -> 
        Invalid "expecting a contract 0 witness"

let private accumulateWeights total witnesses pInputs =
    match pInputs with 
    | [] ->
        accumulateContractWeight total witnesses pInputs
    | PointedOutput (_, output) :: _ ->
        match output.lock with
        | Coinbase _
        | PK _
        | Vote _ ->
            accumulatePkWeight total witnesses pInputs
        | Contract _ ->
            accumulateContractWeight total witnesses pInputs
        | _ -> failwith "unexpected output type"
    | Mint _ :: _ ->
        accumulateContractWeight total witnesses pInputs
  
let rec private validateNext state =
    match state with
    | Invalid err -> Invalid err
    | NextInput (total, witnesses, inputs) ->
        accumulateWeights total witnesses inputs
        |> validateNext
    | Valid totalWeight -> Valid totalWeight

let private transactionSizeWeight =
    Transaction.serialize Full 
    >> Seq.length
    >> uint32
    >> (*) sizeFactor
    >> bigint
    
let transactionWeight tx txSkeleton = result {
    let witnesses = tx.witnesses
    let inputs = txSkeleton.pInputs

    return! match validateNext (NextInput (transactionSizeWeight tx, witnesses, inputs)) with
            | Valid total -> Ok total
            | Invalid error -> Error error
            | NextInput _ -> failwith "unexpected"
}

let blockWeight getUTXO block set =
    block.transactions
    |> List.fold (fun state ex -> 
        state
        >>= (fun (total, set) ->
            UtxoSet.tryGetOutputs getUTXO set ex.tx.inputs
            |> Result.ofOption "could not get outputs"
            <@> TxSkeleton.fromTransaction ex.tx
            >>= transactionWeight ex.tx
            >>= (fun weight ->
                try //TODO: convert to result
                    let set = UtxoSet.handleTransaction getUTXO ex.txHash ex.tx set
                    Ok (weight + total, set)
                with _ as ex when ex.Message = "Expected output to be unspent" ->
                    Error "could not get outputs"
        ))) (Ok (0I, set))
    <@> fst