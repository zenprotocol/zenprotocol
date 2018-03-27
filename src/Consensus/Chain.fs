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
        genesisHash= get <| Hash.fromString "adfb6e07501a9b183f05b60343a2b8cb4375b64a5e17a54af18a56aa7ab8f670";
        genesisTime=1517828985040UL
        networkId=1001ul
        contractSacrificePerBytePerBlock=ContractSacrificePerBytePerBlock
    }

let localParameters = {
    testParameters with
        name="local"
        genesisHash =
            get <| Hash.fromString "7ffa8c6b1525b8b98ba7847a524a0383659d111d793d5249f4b39b0c84d06b4c";
        genesisTime=1515594186383UL
        networkId=1002ul
}

let getChainParameters = function
    | Main -> mainParameters
    | Test -> testParameters
    | Local -> localParameters