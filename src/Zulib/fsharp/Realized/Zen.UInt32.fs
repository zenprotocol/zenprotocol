module Zen.UInt32

open Zen.Cost
open Zen.Array

module Arr = Microsoft.FSharp.Collections.Array

let of_byte (x:FStar.UInt8.byte) : cost<FStar.UInt32.t, Prims.unit>=
  int64 x |> ret

let to_byte_array (x:FStar.UInt32.t)
  :cost<array<FStar.UInt8.byte, Prims.unit>, Prims.unit> =
  let sysbytes' = System.BitConverter.GetBytes x
  let sysbytes =  if System.BitConverter.IsLittleEndian
                    then Arr.rev sysbytes'
                  else sysbytes'
  sysbytes |> Arr.map int |> A |> ret

let of_byte_array (A a:array<FStar.UInt8.byte, Prims.unit>)
  : cost<FStar.UInt32.t, Prims.unit>=
  let a' = a |> Arr.map byte
  System.BitConverter.ToUInt32(a', 0) |> int64 |> ret
