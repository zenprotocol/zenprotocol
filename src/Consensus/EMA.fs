module Consensus.EMA
open Consensus.Chain
open Consensus.Types

type T  = {
    difficulty: uint32;
    delayed: uint64 list;
}

[<Literal>]
let adjustment = 0.984

let clamp lower upper x = min upper (max lower x)

let create (chain:ChainParameters) =
    let difficulty = Difficulty.compress chain.proofOfWorkLimit
    {difficulty = difficulty; delayed=[]}

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
    let alpha = chain.smoothingFactor
    let currentInterval = float <| (bigint newDelayed.[10]) - (bigint newDelayed.[9])

    let currentTarget = Hash.toBigInt <| Difficulty.uncompress ema.difficulty
    let estimate = float currentTarget * ((1.0-alpha) + (alpha * currentInterval) / (float chain.blockInterval * adjustment))
    let nextTarget = clamp  (Hash.toBigInt <| Difficulty.maximum)   // maximum difficulty => low target
                            (Hash.toBigInt <| chain.proofOfWorkLimit)   // high target
                            (bigint estimate)
    let nextDifficulty = Difficulty.compress <| Hash.fromBigInt nextTarget
    {ema with delayed = newDelayed; difficulty = nextDifficulty}

let earliest ema =
    //TODO: Refactor genesis block handling so the first case can raise an error.
    if List.isEmpty ema.delayed
        then 0UL
    elif List.length ema.delayed < 11
        then List.head ema.delayed
    else
        median ema.delayed
