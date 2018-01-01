module Consensus.Block

open Consensus

module Difficulty =   
    let compress (Hash.Hash h) =            
        // Counting the number of bytes representing the number         
        let length = Array.fold (fun state b -> 
            match state with 
            | 0 -> if b = 0uy then 0 else 1
            | _ -> state + 1) 0 h                    
    
        let exp = uint32 (length) <<< 24
    
        match length with
        | 1 ->  (uint32 h.[Hash.Length - 1]) + exp
        | 2 ->  (uint32 h.[Hash.Length - 1]) + ((uint32 h.[Hash.Length - 2]) <<< 8) + exp
        | _ -> 
            let i = Hash.Length - length
            (uint32 h.[i + 2]) + ((uint32 h.[i + 1]) <<< 8) + (uint32 (h.[i]) <<< 16) + exp
            
    let uncompress (target:uint32) =    
        let exp = (int (target >>> 24))
        let mantissa = [|  byte ((target >>> 16) &&& 0xFFu) ; byte ((target >>> 8) &&& 0xFFu);byte (target &&& 0xFFu)|]
    
        let length = if exp < 3 then 0 else exp - 3;
    
        Hash.Hash <| Array.concat [Array.create (Hash.Length - length - 3) 0uy; mantissa; Array.create length 0uy;]