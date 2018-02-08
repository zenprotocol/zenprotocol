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
    | Local -> {proofOfWorkLimit=Difficulty.uncompress 0x20fffffful;blockInterval=60UL*1000UL;smoothingFactor=0.05}
    
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
    | Test -> 
        Hash.fromString "9c38be3ee5e1a3d6e3c4f7184ff1b1cc99b44dfa12ce2cfc8ba437eeaa33627a" |>
        function | Ok value -> value | Error error -> failwith error        
    | Local -> 
        Hash.fromString "53daa9610424738861298485486067be18c4f03358f3ee41e676d7f07ef4497e" |>
        function | Ok value -> value | Error error -> failwith error   
    
let getGenesisTime = 
    function 
    | Main -> 0UL
    | Test -> 1517828985040UL
    | Local -> 1515594186383UL
    