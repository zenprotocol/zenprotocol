module Zen.Dictionary

module Cost = Zen.Cost.Realized

type dictionary<'a> =
    // A map from strings to `'a`, plus the number of elements in the set
    Map<string, 'a> * uint32

type t<'a> = dictionary<'a>

let empty : dictionary<'a> = Map.empty, 0u

let add (s:string) (x:'a) ((d, n):dictionary<'a>)
    : Cost.t<dictionary<'a>, unit> =
    lazy (
        if d |> Map.containsKey s then Map.add s x d, n
        elif n <= System.UInt32.MaxValue - 1u then Map.add s x d, n+1u
        else failwith "Exceeded max dictionary size"
    )
    |> Cost.C

let containsKey (s:string) (d:dictionary<'a>) : Cost.t<bool, unit> =
    lazy ( Map.containsKey s (fst d) )
    |> Cost.C

let remove (s:string) ((d, n):dictionary<'a>) : Cost.t<dictionary<'a>, unit> =
    lazy (
        if not (d |> Map.containsKey s) then d, n
        else Map.remove s d, n - 1u
    )
    |> Cost.C

let tryFind (s:string) ((d, _):dictionary<'a>)
    : Cost.t<FStar.Pervasives.Native.option<'a>, unit> =
    lazy (
        d |> Map.tryFind s
          |> function | None -> FStar.Pervasives.Native.None
                      | Some x -> FStar.Pervasives.Native.Some x
    )
    |> Cost.C
