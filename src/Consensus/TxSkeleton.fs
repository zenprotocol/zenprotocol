module Consensus.TxSkeleton

open Consensus.Types

type TxSkeleton = {
    pInputs: PointedOutput list
    outputs: Output list
}

let empty = 
    {
        pInputs = []
        outputs = []
    }
    
let addInputs inputs (txSkeleton:TxSkeleton) =     
    {txSkeleton with pInputs=List.append txSkeleton.pInputs inputs}
    
let addOutput output (txSkeleton:TxSkeleton) = 
    {txSkeleton with outputs=List.append txSkeleton.outputs [output]}    
    
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

let fromTransaction tx outputs =
    let (==) a b =
        List.length a = List.length b

    if tx.inputs == outputs then
        Ok {
            pInputs = List.zip tx.inputs outputs
            outputs = tx.outputs
        }
    else
        Error "could not construct txSkeleton"

let applyMask tx cw =
    let (=>) list length = 
        List.length list >= length

    if tx.pInputs => cw.inputsLength &&
       tx.outputs => cw.outputsLength then
        Ok { 
            tx with 
                pInputs = tx.pInputs.[cw.beginInputs - 1 .. cw.inputsLength - 1]
                outputs = tx.outputs.[cw.beginOutputs - 1 .. cw.outputsLength - 1]
        }
    else
        Error "could not apply mask"


let isSkeletonOutpoint outpoint = 
    outpoint.txHash = Hash.zero 
    && outpoint.index = 0ul

let isSkeletonOf txSkeleton tx =
    tx.inputs = (
        txSkeleton.pInputs
        |> List.filter (fun (input, _) -> 
            not <| isSkeletonOutpoint input)
        |> List.map fst)
    && tx.outputs = txSkeleton.outputs
