module Consensus.Tests.EMATests

open Consensus
open Consensus.Types
open Consensus.Tests
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open EMA
open FsUnit

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

[<Test>]
let ``Clamp returns a value between lower and upper``() =
    clamp 10 100 50 |> should equal 50
    clamp 10 100 5 |> should equal 10
    clamp 10 100 200 |> should equal 100
    clamp 10I 100I 50I |> should equal 50I

[<Property(Arbitrary = [|typeof<NormalIntervalTimestamps>|], StartSize=50)>]
let ``Default difficulty for first 11 blocks``(tstamps:uint64 list) =
    let ema = create Chain.mainParameters
    let emas = List.scan (fun e ts -> add Chain.mainParameters ts e) ema (List.take 11 tstamps)
    List.forall (fun e -> e.difficulty = ema.difficulty) emas

type FastIntervalTimestamps =
    static member TimestampArb() = Arb.fromGen (timestampGen 180.)

[<Property(Arbitrary = [|typeof<FastIntervalTimestamps>|], StartSize=1000)>]
let ``Difficulty increases when blocks are generated quickly``(tstamps:uint64 list) =
    let ema = create Chain.mainParameters
    let emas = List.scan (fun e ts -> add Chain.mainParameters ts e) ema tstamps
    let initTarget = Hash.toBigInt Chain.mainParameters.proofOfWorkLimit
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
    let ema = create Chain.mainParameters
    let emas = List.scan (fun e ts -> add Chain.mainParameters ts e) ema tstamps
    let targetLimit = Hash.toBigInt Chain.mainParameters.proofOfWorkLimit
    List.forall
        (fun e ->
            e.difficulty
            |> Difficulty.uncompress
            |> Hash.toBigInt <= targetLimit)
        emas
