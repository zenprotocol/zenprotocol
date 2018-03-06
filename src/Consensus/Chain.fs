module Consensus.Chain
open Infrastructure.Result

let ContractSacrificePerBytePerBlock = 1UL

type Chain =
    | Main
    | Local
    | Test

type ChainParameters =
    {
        proofOfWorkLimit:Hash.Hash;
        blockInterval:uint64;
        smoothingFactor:float;
        maxBlockWeight:bigint;
        sacrificePerByteBlock:uint64;
        genesisHash:Hash.Hash;
        genesisTime:uint64
    }

let mainParameters =
    {   proofOfWorkLimit=Difficulty.uncompress 0x1d00fffful;
        blockInterval=236682UL;
        smoothingFactor=0.0055;
        maxBlockWeight=1000_000_000I;
        sacrificePerByteBlock=1UL;
        genesisHash=Hash.zero
        genesisTime=0UL
    }

let testParameters =
    {   proofOfWorkLimit=Difficulty.uncompress 0x20fffffful;
        blockInterval=60UL*1000UL;
        smoothingFactor=0.05;
        maxBlockWeight=1000_000_000I;
        sacrificePerByteBlock=1UL;
        genesisHash= get <| Hash.fromString "9c38be3ee5e1a3d6e3c4f7184ff1b1cc99b44dfa12ce2cfc8ba437eeaa33627a";
        genesisTime=1517828985040UL
    }

let localParameters = {
    testParameters with
        genesisHash =
            get <| Hash.fromString "7ffa8c6b1525b8b98ba7847a524a0383659d111d793d5249f4b39b0c84d06b4c";
        genesisTime=1515594186383UL
}

let getChainParameters = function
    | Main -> mainParameters
    | Test -> testParameters
    | Local -> localParameters

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
        Hash.fromString "7ffa8c6b1525b8b98ba7847a524a0383659d111d793d5249f4b39b0c84d06b4c" |>
        function | Ok value -> value | Error error -> failwith error

let getGenesisTime =
    function
    | Main -> 0UL
    | Test -> 1517828985040UL
    | Local -> 1515594186383UL

let getContractSacrificePerBytePerBlock (_:ChainParameters) = ContractSacrificePerBytePerBlock

let getMaximumBlockWeight chain =
    let p = getChainParameters chain
    p.maxBlockWeight