module Consensus.Tally

open Types
open Checked
open Consensus

type T =
    {
        allocation: Map<byte, uint64>
        payout: Map<Recipient * uint64, uint64>
    }

let empty : T = {
        allocation = Map.empty
        payout = Map.empty
    }

let isEmpty = (=) empty

let handleVote adding (vote:VoteData) amount (tally:T) =
    let op = if adding then (+) else (-)

    let accumulate key value map op =
        let defaultValue =
            Map.tryFind key map
            |> Option.defaultValue 0UL
        let newValue = op defaultValue value
        
        if newValue = 0UL then
            Map.remove key map
        else
            Map.add key newValue map

    let tally =
        vote.allocation
        |> Option.map (fun allocation -> { tally with allocation = accumulate allocation amount tally.allocation op })
        |> Option.defaultValue tally

    let tally =
        vote.payout
        |> Option.map (fun payout -> { tally with payout = accumulate payout amount tally.payout op })
        |> Option.defaultValue tally

    tally

// Naive weighted median by sorting and searching (assumes the list of votes is nonempty) 
let private weightedMedian (votes : seq<byte * uint64>) : byte =
    let sortedVotes   = Seq.sortBy fst votes
    let weights       = Seq.map snd sortedVotes
    let totalWeight   = weights |> Seq.sum
    let weightsBefore = Seq.scan (+) 0UL         weights
    let weightsAfter  = Seq.scan (-) totalWeight weights
    let coupledVotes  = Seq.zip3 weightsBefore sortedVotes (Seq.tail weightsAfter)
    let cond (b,_,a)  = 2UL * b <= totalWeight  &&  2UL * a <= totalWeight
    let l, wl         = Seq.find     cond coupledVotes |> fun (_,x,_) -> x
    let h, wh         = Seq.findBack cond coupledVotes |> fun (_,x,_) -> x
    byte ((uint64 l * wl + uint64 h * wh) / (wl + wh))

let private getResultWith aggregator map =
    if Map.isEmpty map then
        None
    else
        map
        |> Map.toSeq
        |> aggregator
        |> Some

let getAllocationResult = getResultWith weightedMedian

let getPayoutResult map = getResultWith (Seq.maxBy snd >> fst) map