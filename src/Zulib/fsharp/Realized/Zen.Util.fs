module Zen.Util

module A = Zen.Array.Realized

(* TODO: add cost, array's size? *)
let hashFromBase64 (b64 : Prims.string) : A.array<byte, unit> =
    System.Text.Encoding.ASCII.GetString b64
    |> System.Convert.FromBase64String

let debug (x:'A) : 'A =
    printfn "%A" x
    x
