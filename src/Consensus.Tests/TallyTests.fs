module Consensus.Tests.TallyTests

open Consensus.Tally
open FsCheck
open FsCheck.NUnit

let private testWeightedMedian (votes : seq<byte * uint64>) (wm : byte) : bool =
    let sortedVotes = Seq.sortBy fst votes
    let lower       = Seq.filter (fun (x,_) -> x < wm) sortedVotes
    let higher      = Seq.filter (fun (x,_) -> x > wm) sortedVotes
    let sumLower    = Seq.sumBy snd lower
    let sumHigher   = Seq.sumBy snd higher
    let sumTotal    = Seq.sumBy snd sortedVotes
    2UL * sumLower <= sumTotal && 2UL * sumHigher <= sumTotal

#if DEBUG

[<Property>]
let ``The weighted median is a weighted median`` (votes : List<byte * uint64>) =
    let votes = Seq.map (fun (x,w) -> (x,w+1UL)) votes in
    (Seq.length votes > 0) ==> lazy (
        testWeightedMedian votes (weightedMedianTest votes)
    )

#endif
