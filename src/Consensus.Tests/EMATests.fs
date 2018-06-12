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
    Gen.elements [0.0 .. 0.0001 .. 1.0]
    |> Gen.where (fun f -> f < 1.)

let exponentialRandomGen (lambda:float) =
    uniformRandomGen
        |> Gen.map (fun f -> -1. * lambda * System.Math.Log(1.-f))

let inline sumListGen (init:^a) (lGen) =
    Gen.map (List.scan (+) init >> List.tail) lGen

let exponentialSumGen n (lambda:float) =
    Gen.listOfLength n (exponentialRandomGen lambda) |> sumListGen 0.

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
let ``Default difficulty for first block``(tstamps:uint64 list) =
    let ema = create Chain.mainParameters
    let emas = List.scan (fun e ts -> add Chain.mainParameters ts e) ema (List.take 1 tstamps)
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

[<Property(Arbitrary = [|typeof<SlowIntervalTimestamps>|], StartSize=50)>]
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

[<Test>]
let ``Difficulty correctly increases on negative timestamp delta``() =
    let delayed =
        [
            1528335066709UL;
            1528335065195UL;
            1528335063259UL;
            1528335062788UL;
            1528335060449UL;
            1528335048423UL;
            1528335047213UL;
            1528335045371UL;
            1528335042051UL;
            1528335037803UL;
            1528335034329UL;]
        |> List.rev

    let ema = {
        difficulty=0x1e29a4c3ul;
        delayed=delayed
    }

    let ema' = EMA.add Chain.testParameters 1528335049864UL ema
    Difficulty.uncompress ema'.difficulty
    |> should be (lessThan <| Difficulty.uncompress ema.difficulty)

let nonPositiveTimestampGen =
    timestampGen 180.

type SometimesNegativeTimestamps =
    // Average interval is 258 * (9/10) - 10 * (1/10)
    static member TimestampArb() =
        let deltaGen backstop lambda =
            Gen.frequency [
                (9,exponentialRandomGen lambda);
                (1,Gen.map (fun f -> -f * backstop) uniformRandomGen)]
        let negTimestampGen backstop forward =
            Gen.sized
            <| fun n ->
                Gen.listOfLength n (Gen.map (fun f -> f*1000.) <| deltaGen backstop forward)
                |> sumListGen 1.e9
                |> Gen.map (List.map ((max 0.) >> uint64)  )
        Arb.fromGen (negTimestampGen (20.) 258.)

[<Property(Arbitrary = [|typeof<SometimesNegativeTimestamps>|], StartSize=300, EndSize=1000, MaxTest=100)>]
let ``Negative timestamps result in sane difficulty``(tstamps:uint64 list) =
    let ema = create Chain.mainParameters
    let emas = List.scan (fun e ts -> add Chain.mainParameters ts e) ema tstamps
    let initTarget = Hash.toBigInt Chain.mainParameters.proofOfWorkLimit
    let finalTarget =
        List.last emas
        |> fun e -> e.difficulty
        |> Difficulty.uncompress
        |> Hash.toBigInt
    let deltas = List.map (fun (t:uint64 ) -> bigint t) tstamps |> List.windowed 2 |> List.map (function | (x::y::_) -> y - x | _ -> failwith "")
    let averageDelta = float <| (List.sum deltas) / (bigint (List.length deltas))
    let expectedDelta = EMA.adjustment * float Chain.mainParameters.blockInterval
    (averageDelta < expectedDelta) ==> (
        (finalTarget < initTarget) |@ "target too high" .&.
        (float (initTarget / finalTarget) < 1000.) |@
        sprintf "target too low final: %A initial: %A (ratio= %A, exp/avg = %f)"
            finalTarget initTarget (initTarget / finalTarget) (expectedDelta / averageDelta))
        