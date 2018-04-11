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
        genesisHash= get <| Hash.fromString "fa9f657a8216fbcadfab5bad334024cedefa455dfe509e01acf9af0c565b0da1";
        genesisTime=1517828985040UL
        networkId=2005ul
        contractSacrificePerBytePerBlock=ContractSacrificePerBytePerBlock
    }

let localParameters = {
    testParameters with
        name="local"
        genesisHash =
            get <| Hash.fromString "07473faf36113a546b319f935abecec99ee5d4de305d2ef429db40bee18b0e81";
        genesisTime=1515594186383UL
        networkId=1002ul
}

let getChainParameters = function
    | Main -> mainParameters
    | Test -> testParameters
    | Local -> localParameters
