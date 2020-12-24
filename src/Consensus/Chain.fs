module Consensus.Chain
open Consensus.Types
open FSharp.Compatibility.OCaml
open Infrastructure
open Infrastructure.Result

[<Literal>]
let ContractSacrificePerBytePerBlock = 1UL

[<Literal>]
let PeriodSize = 800_000ul

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
        snapshot:uint32
        nomination:uint32
        allocationCorrectionCap:byte
        cgpContractId:ContractId
        votingContractId:ContractId
        upperAllocationBound: byte
        thresholdFactor: uint64 * uint64
        genesisTotal: uint64
    }

let mainParameters =
    {
        name="main"
        proofOfWorkLimit=Difficulty.uncompress 0x1c1ddec6ul;
        blockInterval=236682UL;
        smoothingFactor=28I;
        maxBlockWeight=8_000_000_000I;
        sacrificePerByteBlock=1UL;
        genesisHashHash=Hash.fromString "eea8718b5edf1f621cd6e495a6b2f0aada2b18f075aa0159d55ee648279b3c5e" |> Option.get;
        genesisTime= new System.DateTime(2018,6,30,17,0,0,System.DateTimeKind.Utc) |> Infrastructure.Timestamp.fromDateTime // 1530378000000UL
        networkId=1000ul
        contractSacrificePerBytePerBlock=ContractSacrificePerBytePerBlock
        versionExpiry= new System.DateTime(2021,2,15,0,0,0,System.DateTimeKind.Utc) |> Infrastructure.Timestamp.fromDateTime
        intervalLength=10000ul
        snapshot=9000ul
        nomination=500ul
        allocationCorrectionCap=15uy
        coinbaseMaturity=100ul
        cgpContractId= Option.get <| ContractId.fromString "00000000cdaa2a511cd2e1d07555b00314d1be40a649d3b6f419eb1e4e7a8e63240a36d1"
        votingContractId= Option.get <| ContractId.fromString "000000006ea5457ed23e3e13f31fe4cfd46c200587f2e4cc22df30ac77790f6d2c15cc12"
        upperAllocationBound=90uy
        thresholdFactor=(3UL, 100UL)
        genesisTotal=20_000_000UL * 100_000_000UL
    }

let testParameters =
    {
        name="testnet"
        proofOfWorkLimit=Difficulty.uncompress 0x1dfffffful;
        blockInterval=236682UL;
        smoothingFactor=28I;
        maxBlockWeight=8_000_000_000I;
        sacrificePerByteBlock=1UL;
        genesisHashHash =
            Hash.fromString "5488069e4be0551a3c886543845c332633731c536853209c2dbe04c035946490"
            |> Option.get
            |> Hash.computeOfHash
        genesisTime=1535968146719UL
        networkId=2016ul
        contractSacrificePerBytePerBlock=ContractSacrificePerBytePerBlock
        versionExpiry= new System.DateTime(2200,1,1,0,0,0,System.DateTimeKind.Utc) |> Infrastructure.Timestamp.fromDateTime
        intervalLength=100ul
        snapshot=90ul
        nomination=5ul
        allocationCorrectionCap=15uy
        coinbaseMaturity=10ul
        cgpContractId=Option.get <| ContractId.fromString "00000000eac6c58bed912ff310df9f6960e8ed5c28aac83b8a98964224bab1e06c779b93"
        votingContractId= Option.get <| ContractId.fromString "00000000e89738718a802a7d217941882efe8e585e20b20901391bc37af25fac2f22c8ab" 
        upperAllocationBound=90uy
        thresholdFactor=(3UL, 100UL)
        genesisTotal=1UL
    }

let localGenesisHash = Hash.fromString "6d678ab961c8b47046da8d19c0de5be07eb0fe1e1e82ad9a5b32145b5d4811c7" |> Option.get

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
        thresholdFactor=(1UL, 500UL)
}

let getChainParameters = function
    | Main -> mainParameters
    | Test -> testParameters
    | Local -> localParameters
    
let getChain (chainParams: ChainParameters) =
    match chainParams.name with
    | "main" -> Main
    | "testnet" -> Test
    | _ -> Local

let getPeriod blockNumber =
    if blockNumber < 2ul then 0ul
     else (blockNumber - 2ul) / 800_000ul

let getBlockInPeriod blockNumber =
    if blockNumber < 2ul then 0ul
    else (blockNumber - 2ul) % 800_000ul

let initialBlockReward = 50UL * 100_000_000UL

let blockReward blockNumber (allocationPortion : byte) =
    let allocation = 100UL - uint64 allocationPortion
    
    let initial = (initialBlockReward * allocation) / 100UL
    initial >>> int (getPeriod blockNumber)

let blockTotalReward blockNumber =
    initialBlockReward >>> int (getPeriod blockNumber)

let blockAllocation (blockNumber:uint32) allocationPortion =
    (blockTotalReward blockNumber) - (blockReward blockNumber allocationPortion)    

let getCurrentZPIssuance chainParams (blockNumber:uint32) =
    if blockNumber < 2ul then
        chainParams.genesisTotal
    else
        let period = getPeriod blockNumber
        if period = 0ul then
            chainParams.genesisTotal + initialBlockReward * uint64 (blockNumber - 1ul)
        else
            let sumOfKalapasPerPeriod =
                if period < 10ul then
                    //    Up to period 10 we can simplify the sum
                    (initialBlockReward <<< 1) - (initialBlockReward >>> (int period - 1))
                else
                    //    From period 10 onwards the simplification doesn't work
                    //    because it assumes fractions of Kalapas
                    let first9Periods =
                        (initialBlockReward <<< 1) - (initialBlockReward >>> 8)
                    let period10onwards =
                        seq { for k in 9 .. (int period - 1) do yield initialBlockReward >>> k }
                        |> Seq.sum
                    first9Periods + period10onwards
            let pastPeriods =
                uint64 PeriodSize * sumOfKalapasPerPeriod
            let currentPeriod =
                uint64 (getBlockInPeriod blockNumber + 1ul) * (initialBlockReward >>> int period)
            chainParams.genesisTotal + pastPeriods + currentPeriod
