module Zen.Util

let debug (x:'A) : 'A =
    printfn "%A" x
    x
