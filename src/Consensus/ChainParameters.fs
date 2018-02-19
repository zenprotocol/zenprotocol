module Consensus.ChainParameters

open Consensus.Types

let ContractSacrificePerBytePerBlock = 1UL

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
        Hash.fromString "13319403fd078d87a2f91543a4d1ed2c0d8fe64b0657b123e709a362d59a3fb6" |>
        function | Ok value -> value | Error error -> failwith error

let getGenesisTime =
    function
    | Main -> 0UL
    | Test -> 1517828985040UL
    | Local -> 1515594186383UL

let getContractSacrificePerBytePerBlock (_:Chain) = ContractSacrificePerBytePerBlock