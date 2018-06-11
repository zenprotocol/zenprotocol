module Consensus.Weight

open Consensus.Types
open TxSkeleton
open Infrastructure
open Infrastructure.Result
open Serialization

let result = new ResultBuilder<string>()

//Literal
let activationFactor = 300000u


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
    activation : Consensus.Types.Contract -> bigint
    dataSize : bigint
}

// Returns outputs without mints. But they get added back from the inputs.
let getOutputs = UtxoSet.tryGetOutputs

let pkWitnessWeight : bigint * int * int = 100_000I, 1, 0

let contractWitnessWeight (cWit:ContractWitness) =
    let signatureWeight =
        match cWit.signature with
        | Some _ -> 80_000I
        | None -> 0I

    let costWeight = 100I * bigint (cWit.cost)

    signatureWeight + costWeight, int cWit.inputsLength, int cWit.outputsLength

let activationWeight contract =
    match contract with
    | V0 contract ->
        contract.queries * contract.rlimit |> bigint
    | HighV _ -> 
        0I
    
let realWeights =
    {   pk=pkWitnessWeight;
        contract=contractWitnessWeight;
        activation=activationWeight;
        dataSize=0I         // Size of transaction not counted for now.
    }

        
let updateSkeleton (skeleton, currentSkeleton) nInputs nOutputs =
    if nInputs > List.length skeleton.pInputs || 
       nOutputs > List.length skeleton.outputs
    then
       Error "invalid contract witness"
    else
        match (List.splitAt nInputs skeleton.pInputs,
               List.splitAt nOutputs skeleton.outputs) with
        | (inputsDone, inputs), (outputsDone, outputs) ->
            Ok ({pInputs = inputs; outputs = outputs},
             {pInputs = List.append currentSkeleton.pInputs inputsDone;
              outputs = List.append currentSkeleton.outputs outputsDone})

let accumulateWeights weights fixedSkeleton =
    let rec accumulateWeights skeletonPair total witnesses = result {
        let! skeleton, currentSkeleton = skeletonPair
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
                        (updateSkeleton (skeleton, currentSkeleton) nInputs nOutputs)
                        (total+weight)
                        ws
    }
    accumulateWeights (Ok (fixedSkeleton, TxSkeleton.empty)) 0I

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
    let contractActivationWeight =
        match tx.contract with
        | None -> 0I
        | Some contract -> weights.activation contract
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
// TODO: don't use utxo set here
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