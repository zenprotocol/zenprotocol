module Consensus.TxSkeleton

open Consensus.Types

type PointedOutput = Outpoint * Output

type TxSkeleton = {
    inputs: Outpoint list //TODO: PointedOutput list
    outputs: Output list
}

let empty = 
    {
        inputs = []
        outputs = []
    }

let checkPrefix (txSub:TxSkeleton) (txSuper:TxSkeleton) = 
    let (=>) (super:List<'a>) (sub:List<'a>) = 
        let subLen, superLen = List.length sub, List.length super
        superLen >= subLen && super.[superLen-subLen..superLen-1] = sub

    if txSuper.inputs  => txSub.inputs &&
       txSuper.outputs => txSub.outputs then
        Ok txSuper
    else
        Error "invalid prefix"