module FStar.Char

open Zen.Cost
module U8 = FStar.UInt8

type char = U8.t
type t = char

let lowercase (c:char) : char `cost` 2 = let open U8 in
    if c >=^ 65uy && c <=^ 90uy
    then incRet 2 (c +^ 32uy)
    else incRet 2 c

let uppercase (c:char) : char `cost` 2 = let open U8 in
    if c >=^ 97uy && c <=^ 122uy
    then incRet 2 (c -^ 32uy)
    else incRet 2 c
