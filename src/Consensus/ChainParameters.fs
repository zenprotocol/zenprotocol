module Consensus.ChainParameters

open Consensus.Types

type Chain =
    | Main
    | Test

type ChainParameters = 
    {
        proofOfWorkLimit:Hash.Hash;
        blockInterval:uint64;
        smoothingFactor:float
    }

let getChainParameters = function
    | Main -> {proofOfWorkLimit=Difficulty.uncompress 0x1d00fffful;blockInterval=1000UL;smoothingFactor=0.0055}
    | Test -> {proofOfWorkLimit=Difficulty.uncompress 0x20fffffful;blockInterval=1000UL;smoothingFactor=0.05}

let proofOfWorkLimit chain =
    let p = getChainParameters chain
    p.proofOfWorkLimit

let blockInterval chain =
    let p = getChainParameters chain
    p.blockInterval

let smoothingFactor chain =
    let p = getChainParameters chain
    p.smoothingFactor