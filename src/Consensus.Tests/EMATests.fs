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

let seqGen n (itemGen:Gen<'a>) =
    Gen.sequenceToSeq <| Seq.init n (fun _ -> itemGen)

let sumSeqGen (seqGen:Gen<seq<float>>) =
    Gen.map (Seq.scan (+) 0.) seqGen |> Gen.map (Seq.skip 1)

let exponentialSumGen n (lambda:float) =
    exponentialRandomGen lambda |> seqGen n |> sumSeqGen

let timestampGen = Gen.map (Seq.map (fun f -> (uint64 (f * 1000.)))) (exponentialSumGen 500 240.)

type TimestampSequence =
    static member TimestampArb() = Arb.fromGen timestampGen

[<Property(Arbitrary = [|typeof<TimestampSequence>|])>]
let ``Default difficulty for first 11 blocks``(tstamps:seq<uint64>) =
    let ema = create ChainParameters.Main
    let emas = Seq.scan (fun e ts -> add ChainParameters.Main ts e) ema (Seq.take 11 tstamps)
    Seq.forall (fun e -> e.difficulty = ema.difficulty) emas