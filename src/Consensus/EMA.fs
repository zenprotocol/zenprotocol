module Consensus.EMA
open Consensus.Types

type T  = {
    difficulty: uint32;
    delayed: BlockHeader list;
}

let create chain = 
    let difficulty = Difficulty.compress (ChainParameters.proofOfWorkLimit chain)     
    {difficulty = difficulty; delayed=[]}

let add header ema =
    ema
    
let get ema = ema.difficulty 
                    