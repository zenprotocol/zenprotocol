module Consensus.ChainParameters

open Consensus.Types

type Chain =
    | Main
    | Local
    | Test

type ChainParameters = 
    {
        proofOfWorkLimit:Hash.Hash;
        blockInterval:uint64;
        smoothingFactor:float
    }

let getChainParameters = function
    | Main -> {proofOfWorkLimit=Difficulty.uncompress 0x1d00fffful;blockInterval=240UL*1000UL;smoothingFactor=0.0055}
    | Test
    | Local -> {proofOfWorkLimit=Difficulty.uncompress 0x20fffffful;blockInterval=240UL*1000UL;smoothingFactor=0.05}
    
let proofOfWorkLimit chain =
    let p = getChainParameters chain
    p.proofOfWorkLimit

let blockInterval chain =
    let p = getChainParameters chain
    p.blockInterval

let smoothingFactor chain =
    let p = getChainParameters chain
    p.smoothingFactor
    
let getGenesisHash = 
    function
    | Main -> Hash.zero
    | Test -> Hash.zero
    | Local -> 
        match Hash.fromString "53daa9610424738861298485486067be18c4f03358f3ee41e676d7f07ef4497e" with
        | Ok value -> value
        | Error err -> 
            Infrastructure.Log.error "invalid genesis hash"
            Hash.zero
    
let getGenesisTime = 
    function 
    | Main -> 0UL
    | Test -> 0UL
    | Local -> 1515594186383UL
    