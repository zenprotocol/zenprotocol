module Consensus.EMA
open Consensus.Chain

type T  = {
    difficulty: uint32;
    delayed: uint64 list;
}

[<Literal>]
let adjustment = 0.982

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

let nextDifficultyTarget targetInterval smoothing solveTime difficulty =
    let currentDifficultyTarget = Hash.toBigInt <| Difficulty.uncompress difficulty
    let t = bigint (decimal targetInterval * decimal adjustment)
    let st = clamp -(15I * 60I * 1000I) (6I * t) solveTime
    currentDifficultyTarget * (smoothing * t - t + st) / (smoothing * t)    // st large => raise target => lower difficulty

let add chain timestamp ema =
    let newDelayed = push timestamp ema.delayed
    let l = List.length newDelayed
    if l <= 1 then {ema with delayed = newDelayed} else // First block doesn't alter difficulty
    let solveTime = (bigint newDelayed.[l-1]) - (bigint newDelayed.[l-2])     // may be negative
    let unclampedNextTarget = nextDifficultyTarget chain.blockInterval chain.smoothingFactor solveTime ema.difficulty
    let nextTarget = clamp  (Hash.toBigInt <| Difficulty.maximum)   // maximum difficulty => low target
                            (Hash.toBigInt <| chain.proofOfWorkLimit)   // high target
                            unclampedNextTarget
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
