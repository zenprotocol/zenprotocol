module Consensus.EMA
open Consensus.Types

type T  = {
    difficulty: uint32;
    interval: uint64;
    delayed: uint64 list;
    chain: ChainParameters.Chain
}

let create chain =
    let difficulty = Difficulty.compress (ChainParameters.proofOfWorkLimit chain)
    {difficulty = difficulty; interval=ChainParameters.blockInterval chain; delayed=[];chain=chain}

let push newTimestamp (timestamps:uint64 list) =
    if List.length timestamps < 11 then
        timestamps @ [newTimestamp]
    else
        (List.tail timestamps) @ [newTimestamp]

let median timestamps =
    if List.isEmpty timestamps then
        (System.DateTime.Parse "2018-01-01T00:00:0.0000000").Ticks |> uint64     // Alter to approximate true genesis timestamp
    else
        List.item (List.length timestamps / 2) (List.sort timestamps)

let add header ema =
    let oldDelayed = ema.delayed
    let newDelayed = push header ema.delayed
    let alpha = ChainParameters.smoothingFactor ema.chain
    let currentInterval = median newDelayed - median oldDelayed
    let nextEstimatedInterval = float ema.interval * (1.0 - alpha) + float currentInterval * alpha |> uint64
    let currentTarget = Hash.toBigInt <| Difficulty.uncompress ema.difficulty
    let nextTarget = currentTarget * bigint nextEstimatedInterval / bigint ema.interval
    let nextDifficulty = Difficulty.compress <| Hash.fromBigInt nextTarget
    {ema with delayed = newDelayed; interval = nextEstimatedInterval; difficulty = nextDifficulty}

let get ema = ema.difficulty
