module Consensus.Tests.EMATests

open Consensus
open Consensus.Types
open Consensus.Tests
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open EMA

let uniformRandomGen =
    Arb.generate<NormalFloat>
        |> Gen.map float
        |> Gen.resize 1
        |> Gen.map abs
        |> Gen.where (fun f -> f < 1.)

let exponentialRandomGen (lambda:float) =
    uniformRandomGen
        |> Gen.map (fun f -> -1. * lambda * System.Math.Log(1.-f))

let sumListGen (lGen:Gen<list<float>>) =
    Gen.map (List.scan (+) 0. >> List.tail) lGen

let exponentialSumGen n (lambda:float) =
    Gen.listOfLength n (exponentialRandomGen lambda) |> sumListGen

let timestampGen blocktime =
    Gen.sized <| 
        fun s -> 
            Gen.map (List.map (fun f -> (uint64 (f * 1000.))))
                    (exponentialSumGen s blocktime)

type NormalIntervalTimestamps =
    static member TimestampArb() = Arb.fromGen (timestampGen 240.)

[<Property(Arbitrary = [|typeof<NormalIntervalTimestamps>|], StartSize=50)>]
let ``Default difficulty for first 11 blocks``(tstamps:uint64 list) =
    let ema = create ChainParameters.Main
    let emas = List.scan (fun e ts -> add ChainParameters.Main ts e) ema (List.take 11 tstamps)
    List.forall (fun e -> e.difficulty = ema.difficulty) emas

type FastIntervalTimestamps =
    static member TimestampArb() = Arb.fromGen (timestampGen 180.)

[<Property(Arbitrary = [|typeof<FastIntervalTimestamps>|], StartSize=1000)>]
let ``Difficulty increases when blocks are generated quickly``(tstamps:uint64 list) =
    let ema = create ChainParameters.Main
    let emas = List.scan (fun e ts -> add ChainParameters.Main ts e) ema tstamps
    let initTarget = Hash.toBigInt (ChainParameters.proofOfWorkLimit ChainParameters.Main)
    let finalTarget =
        List.last emas
        |> fun e -> e.difficulty
        |> Difficulty.uncompress
        |> Hash.toBigInt
    finalTarget < initTarget

type SlowIntervalTimestamps =
    static member TimestampArb() = Arb.fromGen (timestampGen 300.)

[<Property(Arbitrary = [|typeof<SlowIntervalTimestamps>|], StartSize=1000)>]
let ``Difficulty is always more than the chain minimum``(tstamps:uint64 list) =
    let ema = create ChainParameters.Main
    let emas = List.scan (fun e ts -> add ChainParameters.Main ts e) ema tstamps
    let targetLimit = Hash.toBigInt (ChainParameters.proofOfWorkLimit ChainParameters.Main)
    List.forall
        (fun e -> 
            e.difficulty
            |> Difficulty.uncompress
            |> Hash.toBigInt <= targetLimit)
        emas
