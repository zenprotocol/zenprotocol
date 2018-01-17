module Consensus.TxSkeleton

open Consensus.Types

type PointedOutput = Outpoint * Output

type TxSkeleton = {
    pInputs: PointedOutput list
    outputs: Output list
}

let empty = 
    {
        pInputs = []
        outputs = []
    }

let checkPrefix txSub txSuper = 
    let (=>) (super:List<'a>) (sub:List<'a>) = 
        let subLen, superLen = List.length sub, List.length super
        superLen >= subLen && super.[superLen-subLen..superLen-1] = sub

    if txSuper.pInputs  => txSub.pInputs &&
       txSuper.outputs => txSub.outputs then
        Ok txSuper
    else
        Error "invalid prefix"

let applyMask (tx:Types.Transaction) witness outputs : TxSkeleton =
    empty //TODO

let fromTransaction (tx:Types.Transaction) outputs : TxSkeleton =
    {
        pInputs = List.zip tx.inputs outputs
        outputs = tx.outputs
    }

let isSkeletonOf txSkeleton tx =
    tx.inputs = (
        txSkeleton.pInputs
        |> List.filter (fun (input, _) -> 
            not <| (input.txHash = Hash.zero && input.index = 0ul))
        |> List.map fst)
    && tx.outputs = txSkeleton.outputs