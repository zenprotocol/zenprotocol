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