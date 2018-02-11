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

let private median timestamps =
    if List.isEmpty timestamps then
        failwith "Can't find median of an empty list" // should never happen
    else
        List.item (List.length timestamps / 2) (List.sort timestamps) // inaccurate on even lengths, doesn't matter

let add chain header ema =
    let newDelayed = push header ema.delayed
    if List.length ema.delayed < 11 then {ema with delayed = newDelayed} else // First 11 blocks don't alter difficulty
    let alpha = ChainParameters.smoothingFactor chain
    let oldDelayed = ema.delayed
    let currentInterval = median newDelayed - median oldDelayed
    let nextEstimatedInterval = float ema.interval * (1.0 - alpha) + float currentInterval * alpha |> uint64
    let currentTarget = Hash.toBigInt <| Difficulty.uncompress ema.difficulty
    let nextTarget = clamp  (Hash.toBigInt <| Difficulty.maximum)   // maximum difficulty => low target
                            (Hash.toBigInt <| ChainParameters.proofOfWorkLimit chain)   // high target
                            (currentTarget * bigint nextEstimatedInterval / bigint (ChainParameters.blockInterval chain))
    let nextDifficulty = Difficulty.compress <| Hash.fromBigInt nextTarget
    {ema with delayed = newDelayed; interval = nextEstimatedInterval; difficulty = nextDifficulty}

let earliest ema =
    //TODO: Refactor genesis block handling so the first case can raise an error.
    if List.isEmpty ema.delayed
        then 0UL
    elif List.length ema.delayed < 11
        then List.head ema.delayed
    else
        median ema.delayed
