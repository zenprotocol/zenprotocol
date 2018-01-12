module Consensus.EMA
open Consensus.ChainParameters
open Consensus.Types

type T  = {
    difficulty: uint32;
    interval: uint64;
    delayed: uint64 list;
}

let clamp lower upper x = min upper (max lower x)

let create chain =
    let difficulty = Difficulty.compress (ChainParameters.proofOfWorkLimit chain)
    {difficulty = difficulty; interval=ChainParameters.blockInterval chain; delayed=[]}

let push newTimestamp (timestamps:uint64 list) =
    if List.length timestamps < 11 then
        timestamps @ [newTimestamp]
    else
        (List.tail timestamps) @ [newTimestamp]

let median chain timestamps =
    if List.isEmpty timestamps then
        ChainParameters.getGenesisTime chain
    else
        List.item (List.length timestamps / 2) (List.sort timestamps)

let add chain timestamp ema =
    let median = median chain

    let oldDelayed = ema.delayed
    let newDelayed = push timestamp ema.delayed
    let alpha = ChainParameters.smoothingFactor chain
    let currentInterval = clamp (ema.interval / 10UL) (ema.interval * 10UL) (median newDelayed - median oldDelayed)
    let nextEstimatedInterval = float ema.interval * (1.0 - alpha) + float currentInterval * alpha |> uint64
    let currentTarget = Hash.toBigInt <| Difficulty.uncompress ema.difficulty
    let nextDifficulty = 
        try 
            currentTarget * bigint nextEstimatedInterval / bigint (ChainParameters.blockInterval chain)
            |> Hash.fromBigInt
            |> Difficulty.compress
        with 
        | _ -> 
            ChainParameters.proofOfWorkLimit chain
            |> Difficulty.compress
      
    {ema with delayed = newDelayed; interval = nextEstimatedInterval; difficulty = nextDifficulty}
