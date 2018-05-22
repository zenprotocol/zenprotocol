module Consensus.Chain
open Infrastructure.Result

let ContractSacrificePerBytePerBlock = 1UL

type Chain =
    | Main
    | Local
    | Test

type ChainParameters =
    {
        name:string;
        proofOfWorkLimit:Hash.Hash;
        blockInterval:uint64;
        smoothingFactor:float;
        maxBlockWeight:bigint;
        sacrificePerByteBlock:uint64;
        genesisHash:Hash.Hash;
        genesisTime:uint64
        networkId:uint32;
        contractSacrificePerBytePerBlock:uint64
    }

let mainParameters =
    {
        name="main"
        proofOfWorkLimit=Difficulty.uncompress 0x1d00fffful;
        blockInterval=236682UL;
        smoothingFactor=0.035;
        maxBlockWeight=1000_000_000I;
        sacrificePerByteBlock=1UL;
        genesisHash=Hash.zero
        genesisTime=0UL
        networkId=1000ul
        contractSacrificePerBytePerBlock=ContractSacrificePerBytePerBlock
    }

let testParameters =
    {
        name="testnet"
        proofOfWorkLimit=Difficulty.uncompress 0x20fffffful;
        blockInterval=60UL*1000UL;
        smoothingFactor=0.035;
        maxBlockWeight=1000_000_000I;
        sacrificePerByteBlock=1UL;
        genesisHash= get <| Hash.fromString "62af3061a2733437478f538baad39c12f73fae4be736486ca8d83da41dd1b72c";
        genesisTime=1517828985040UL
        networkId=2007ul
        contractSacrificePerBytePerBlock=ContractSacrificePerBytePerBlock
    }

let localParameters = {
    testParameters with
        name="local"
        genesisHash =
            get <| Hash.fromString "5ef81c5996236650d17d265dd1f18269ecc0c4b2a55934e3f87fb5740c3a98e4";
        genesisTime=1515594186383UL
        networkId=1002ul
}

let getChainParameters = function
    | Main -> mainParameters
    | Test -> testParameters
    | Local -> localParameters
