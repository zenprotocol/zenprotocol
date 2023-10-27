module Consensus.Difficulty

let compress (Hash.Hash h) =
    // Count the number of initial zero bytes
    let zeroes = defaultArg <| Array.tryFindIndex ((<>) 0uy) h <| Hash.Length
    let length = Hash.Length - zeroes
    let exp = uint32 length <<< 24

    match length with
    | 0 -> 0u
    | 1 -> uint32 h.[Hash.Length - 1] + exp
    | 2 -> uint32 h.[Hash.Length - 1] + (uint32 h.[Hash.Length - 2] <<< 8) + exp
    | _ -> uint32 h.[zeroes + 2] + (uint32 h.[zeroes + 1] <<< 8) + (uint32 h.[zeroes] <<< 16) + exp

let uncompress (target:uint32) =
    let shifted_exp = (int (target >>> 24))
    let mantissa = [|  byte ((target >>> 16) &&& 0xFFu) ; byte ((target >>> 8) &&& 0xFFu);byte (target &&& 0xFFu)|]
    let length = min (max shifted_exp 3) Hash.Length

    Hash.Hash <| Array.concat [Array.create (Hash.Length - length) 0uy; mantissa; Array.create (length - 3) 0uy;]
    
[<Literal>]
let maximumCompressed = 0x01000001u

let maximum = uncompress maximumCompressed