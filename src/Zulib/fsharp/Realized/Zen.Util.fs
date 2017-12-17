module Zen.Util

(* TODO: add cost, array's size? *)
let hashFromBase64 = System.Convert.FromBase64String

let debug x =
    printfn "%A" x
    x
