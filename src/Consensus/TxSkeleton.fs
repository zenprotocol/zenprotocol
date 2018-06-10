module Consensus.TxSkeleton

open Consensus
open Consensus.Types

type Input =
    | PointedOutput of PointedOutput
    | Mint of Spend

type T = {
    pInputs: Input list
    outputs: Output list
}

let empty =
    {
        pInputs = []
        outputs = []
    }

let (|Lock|) input =
    match input with
    | PointedOutput (_, {lock=lock}) -> lock
    | Mint {asset=Asset (contractId,_)} -> Contract contractId

let addInputs inputs (txSkeleton:T) =
    {txSkeleton with pInputs=List.append txSkeleton.pInputs inputs}

let addOutput output (txSkeleton:T) =
    {txSkeleton with outputs=List.append txSkeleton.outputs [output]}

let addOutputs outputs (txSkeleton:T) =
    {txSkeleton with outputs=List.append txSkeleton.outputs outputs}

let addChange asset inputsAmount outputsAmount pkHash txSkeleton =
    if inputsAmount > outputsAmount then
        addOutput {lock=PK pkHash;spend={amount=inputsAmount-outputsAmount;asset=asset}} txSkeleton
    else
        txSkeleton

let checkPrefix txSub txSuper =
    let (=>) (super:List<'a>) (sub:List<'a>) =
        let subLen, superLen = List.length sub, List.length super
        superLen >= subLen && super.[0..subLen-1] = sub

    if txSuper.pInputs => txSub.pInputs &&
       txSuper.outputs => txSub.outputs then
        Ok txSuper
    else
        Error "invalid prefix"

//UNDONE: Disappearing check on lengths of pInputs and outputs? make sure.
let fromTransaction tx (outputs : Output List) =
    let pInputs,_ =
        List.fold (fun (inputs, outputs) input ->

            match input with
            | Types.Input.Outpoint outpoint ->
                let output = List.head outputs
                PointedOutput (outpoint, output) :: inputs, List.tail outputs

            | Types.Input.Mint spend ->
                Mint spend :: inputs, outputs

            ) ([], outputs) tx.inputs

    {
        pInputs = List.rev pInputs
        outputs = tx.outputs
    }

let applyMask tx cw =
    let (=>) list length =
        List.length list |> uint32 >= length

    if tx.pInputs => cw.inputsLength &&
       tx.outputs => cw.outputsLength then
        Ok {
            tx with
                pInputs = tx.pInputs.[0 .. int cw.beginInputs - 1 ]
                outputs = tx.outputs.[0 .. int cw.beginOutputs - 1 ]
        }
    else
        Error "could not apply mask"

let isSkeletonOf txSkeleton tx outputs =
    let inputsOfSkeleton =
        txSkeleton.pInputs
        |> List.map (function
            | Mint spend ->
                Types.Input.Mint spend
            | PointedOutput (outpoint, _) ->
                Types.Input.Outpoint outpoint)

    let outputsFromSkeleton =
        txSkeleton.pInputs
        |> List.choose (function
            | Mint _ ->
                None
            | PointedOutput (_, output) ->
                Some output)

    tx.inputs = inputsOfSkeleton // Check that the contract didn't change the inputs
    && outputs = outputsFromSkeleton
    && tx.outputs = txSkeleton.outputs

let getContractWitness contractId command data initialTxSkelton finalTxSkeleton (cost:int64)  =
    let length list = List.length list |> uint32

    {
        contractId = contractId
        command = command
        data = data
        beginInputs = length initialTxSkelton.pInputs
        beginOutputs = length initialTxSkelton.outputs
        inputsLength = length finalTxSkeleton.pInputs - length initialTxSkelton.pInputs
        outputsLength = length finalTxSkeleton.outputs - length initialTxSkelton.outputs
        signature = None
        cost = uint32 cost
    }
