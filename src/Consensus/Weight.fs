module Consensus.Weight

open Consensus.Types
open TxSkeleton
open Infrastructure
open FSharpx.Option
open Infrastructure.Result
open Infrastructure.ZFStar
open Serialization

let result = new ResultBuilder<string>()

//UNDONE: Alter this active pattern when Coinbase locks are generalised.
let (|PKPair|_|) (pInput, witness) =
    match pInput, witness with
    | PointedOutput (_, {lock=(PK _ | Coinbase _)}), PKWitness _ ->
        Some ()
    | _ -> None

let (|ContractPair|_|) (pInput, witness) =
    match pInput, witness with
    | PointedOutput (_, {lock=(Contract _)}), ContractWitness cWitness
    | Mint _, ContractWitness cWitness ->
        Some cWitness
    | _ -> None

[<Struct>]
type Weights = {
    pk : bigint * int * int;
    contract : ContractWitness -> (bigint * int * int)
    activation : string -> Result<bigint,string>
    dataSize : bigint
}

// Returns outputs without mints. But they get added back from the inputs.
let getOutputs = UtxoSet.tryGetOutputs

let pkWitnessWeight : bigint * int * int = 100_000I, 1, 0

let contractWitnessWeight cWit =
    1000I * bigint (cWit.cost), int cWit.inputsLength, int cWit.outputsLength

let activationWeight hints = result {
    let! fuel, iFuel = calculateMetrics hints
    return bigint (fuel + iFuel) * 300_000I
}

let realWeights =
    {   pk=pkWitnessWeight;
        contract=contractWitnessWeight;
        activation=activationWeight;
        dataSize=0I         // Size of transaction not counted for now.
    }

let updateSkeleton (skeleton, currentSkeleton) nInputs nOutputs =
    match  (List.splitAt nInputs skeleton.pInputs,
            List.splitAt nOutputs skeleton.outputs) with
    | (inputsDone, inputs), (outputsDone, outputs) ->
        ({pInputs = inputs; outputs = outputs},
         {pInputs = List.append currentSkeleton.pInputs inputsDone;
          outputs = List.append currentSkeleton.outputs outputsDone}
        )

let accumulateWeights weights fixedSkeleton =
    let rec accumulateWeights skeletonPair total witnesses = result {
        let skeleton, currentSkeleton = skeletonPair
        match skeleton.pInputs, witnesses with
        | [], [] -> return total    // Covers coinbase tranasctions
        | [], _ -> return! Error "Too many witnesses"
        | _, [] -> return! Error "Too few witnesses"
        | pInput::_, witness::ws ->
            let! weight, nInputs, nOutputs =
                match pInput, witness with
                | PKPair -> Ok weights.pk
                | ContractPair cWitness ->
                    Ok <| weights.contract cWitness
                | _ -> Error "Wrong witness type"
            return! accumulateWeights
                        (updateSkeleton skeletonPair nInputs nOutputs)
                        (total+weight)
                        ws
    }
    accumulateWeights (fixedSkeleton, TxSkeleton.empty) 0I

let txWeight weights
        getUTXO memUTXOs tx = result {
    let! outputs =  getOutputs getUTXO memUTXOs tx.inputs
                        |> (|ResultOf|) "Output lookup error"
    let! skeleton =
        try
            Ok <| TxSkeleton.fromTransaction tx outputs
        with
        | e -> Error "Couldn't make a transaction skeleton"
    let! inputWeights = accumulateWeights weights skeleton tx.witnesses
    let! contractActivationWeight =
        match tx.contract with
        | None -> Ok 0I
        | Some (_, hints) -> weights.activation hints
    let txSizeWeight =
        if weights.dataSize = 0I then 0I
        else
            weights.dataSize *
            (Transaction.serialize Full tx
                |> Array.length |> bigint)
    return inputWeights + contractActivationWeight + txSizeWeight
}

let transactionWeight =
    txWeight realWeights


// HACK: Something is wrong with using utxoSet here. Need to work out what.
let bkWeight weights getUTXO utxoSet txs =
    let inner (weight, memUTXOs) (txHash, tx) = result {
        let! txWeight = txWeight weights getUTXO memUTXOs tx
        let newMemUTXOs = UtxoSet.handleTransaction getUTXO txHash tx memUTXOs
        return (weight+txWeight, newMemUTXOs)
    }
    let folder accResult next = result {
        let! acc = accResult
        return! inner acc next
        }
    List.fold
        folder
        (Ok (0I, utxoSet))
        [ for tx in txs do yield Transaction.hash tx, tx ]
    |> Result.map fst

let blockWeight getUTXO utxoSet (block : Block) =
    bkWeight realWeights getUTXO utxoSet (block.transactions)