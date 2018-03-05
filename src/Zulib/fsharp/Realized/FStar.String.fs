module FStar.String

open FSharp.Core.Operators.Checked
type t = Prims.string
module Cost = Zen.Cost.Realized

let strlen (s:t) : Prims.nat = int64 (s.Length)
let length = strlen

let at (s:t) (i:Prims.nat) : t =
    let i = int i in
    let l = s.Length in
    if i < 0 || i >= l then ""B
    else [| s.[i] |]

let strcat (s1:t) (s2:t) : t = Array.append s1 s2
let cat (s1:t) (s2:t) : Cost.t<t, unit> =
    lazy ( strcat s1 s2 ) |> Cost.C
