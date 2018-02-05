module Consensus.TxSkeleton

open Consensus.Types

type T = {
    pInputs: PointedOutput list
    outputs: Output list
}

let empty = 
    {
        pInputs = []
        outputs = []
    }
    
let addInputs inputs (txSkeleton:T) =     
    {txSkeleton with pInputs=List.append txSkeleton.pInputs inputs}
    
let addOutput output (txSkeleton:T) = 
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


let isSkeletonOutpoint outpoint = 
    outpoint.txHash = Hash.zero 
    && outpoint.index = 0ul

let isSkeletonOf txSkeleton tx inputs =
    let withoutSkeletonInputs = 
        txSkeleton.pInputs |> List.filter (fst >> isSkeletonOutpoint >> not)

    tx.inputs = (withoutSkeletonInputs |> List.map fst)
    && inputs = (withoutSkeletonInputs |> List.map snd) // Check that the contract didn't change the inputs
    && tx.outputs = txSkeleton.outputs