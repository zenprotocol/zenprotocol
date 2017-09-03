module Zen.UInt64

open Zen.Cost
open Zen.Array

module Arr = Microsoft.FSharp.Collections.Array

let to_byte_array (x:FStar.UInt64.t)
  :cost<array<FStar.UInt8.byte, Prims.unit>, Prims.unit> =
  let sysbytes' = System.BitConverter.GetBytes x
  let sysbytes =  if System.BitConverter.IsLittleEndian
                    then Arr.rev sysbytes'
                  else sysbytes'
  sysbytes |> Arr.map int |> A |> ret

let of_byte_array (A a:array<FStar.UInt8.byte, Prims.unit>)
  : cost<FStar.UInt64.t, Prims.unit>=
  let a' = a |> Arr.map byte
  System.BitConverter.ToUInt64(a', 0) |> uint64 |> ret
