module Consensus.Chain
open Infrastructure
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
        smoothingFactor:bigint;
        maxBlockWeight:bigint;
        sacrificePerByteBlock:uint64;
        genesisHashHash:Hash.Hash;
        genesisTime:uint64
        networkId:uint32;
        contractSacrificePerBytePerBlock:uint64
        versionExpiry:Timestamp.Timestamp
        coinbaseMaturity:uint32
        intervalLength:uint32
        allocationCorrectionCap:byte
    }

let mainParameters =
    {
        name="main"
        proofOfWorkLimit=Difficulty.uncompress 0x1c1ddec6ul;
        blockInterval=236682UL;
        smoothingFactor=28I;
        maxBlockWeight=2000_000_000I;
        sacrificePerByteBlock=1UL;
        genesisHashHash=Hash.fromString "eea8718b5edf1f621cd6e495a6b2f0aada2b18f075aa0159d55ee648279b3c5e" |> get;
        genesisTime= new System.DateTime(2018,6,30,17,0,0,System.DateTimeKind.Utc) |> Infrastructure.Timestamp.fromDateTime // 1530378000000UL
        networkId=1000ul
        contractSacrificePerBytePerBlock=ContractSacrificePerBytePerBlock
        versionExpiry= new System.DateTime(2020,2,15,0,0,0,System.DateTimeKind.Utc) |> Infrastructure.Timestamp.fromDateTime
        intervalLength=10000ul
        allocationCorrectionCap=50uy
        coinbaseMaturity=100ul
    }

let testParameters =
    {
        name="testnet"
        proofOfWorkLimit=Difficulty.uncompress 0x1dfffffful;
        blockInterval=236682UL;
        smoothingFactor=28I;
        maxBlockWeight=1000_000_000I;
        sacrificePerByteBlock=1UL;
        genesisHashHash =
            Hash.fromString "5488069e4be0551a3c886543845c332633731c536853209c2dbe04c035946490"
            |> get
            |> Hash.computeOfHash
        genesisTime=1535968146719UL
        networkId=2016ul
        contractSacrificePerBytePerBlock=ContractSacrificePerBytePerBlock
        versionExpiry= new System.DateTime(2200,1,1,0,0,0,System.DateTimeKind.Utc) |> Infrastructure.Timestamp.fromDateTime
        intervalLength=100ul
        allocationCorrectionCap=5uy
        coinbaseMaturity=10ul
    }

let localGenesisHash = Hash.fromString "6d678ab961c8b47046da8d19c0de5be07eb0fe1e1e82ad9a5b32145b5d4811c7" |> get

let localParameters = {
    testParameters with
        proofOfWorkLimit=Difficulty.uncompress 0x20fffffful;
        blockInterval=1000UL * 60UL;
        name="local"
        genesisHashHash =
            localGenesisHash
            |> Hash.computeOfHash
        genesisTime=1515594186383UL
        networkId=1002ul
        versionExpiry=System.UInt64.MaxValue
}

let getChainParameters = function
    | Main -> mainParameters
    | Test -> testParameters
    | Local -> localParameters

let private getPeriod blockNumber =
    blockNumber / 800_000ul
    |> int

let private initialBlockReward = 50UL * 100_000_000UL

let blockReward blockNumber (allocationPortion : byte) =
    let allocation = 100UL - uint64 allocationPortion
    
    let initial = (initialBlockReward * allocation) / 100UL
    initial >>> getPeriod blockNumber

let blockAllocation (blockNumber:uint32) allocationPortion =
    (uint64 initialBlockReward >>> getPeriod blockNumber) - (blockReward blockNumber allocationPortion)
