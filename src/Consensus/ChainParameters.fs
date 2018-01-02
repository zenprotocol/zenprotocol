module Consensus.ChainParameters

open Consensus.Types

type Chain = 
    | Main
    | Test

type ChainParameters = 
    {
        proofOfWorkLimit:Hash.Hash
    }
    
let getChainParameters = function
    | Main -> {proofOfWorkLimit=Difficulty.uncompress 0x1d00fffful}
    | Test -> {proofOfWorkLimit=Difficulty.uncompress 0x20fffffful}   
    
let proofOfWorkLimit chain = 
    let p = getChainParameters chain
    p.proofOfWorkLimit