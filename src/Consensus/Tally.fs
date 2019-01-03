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

let getResult map =
    if Map.isEmpty map then
        None
    else
        map
        |> Map.toSeq
        |> Seq.maxBy snd
        |> fst
        |> Some