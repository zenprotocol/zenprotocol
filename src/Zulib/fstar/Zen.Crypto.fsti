module Zen.Crypto

open Zen.Cost
module  A = Zen.Array
module U8 = FStar.UInt8
(*
  http://bench.cr.yp.to/results-hash.html :
  Used to relate costs of crypto functions to number of processor cycles.
  An explanation of how the costs of crypto primitives was chosen is in `Crypto Analysis.nb`
*)

type hash      = Zen.Types.hash
type signature = Zen.Types.signature
type key       = Zen.Types.key

val sha2_256: #n:nat
  -> msg: A.t U8.byte n
  -> cost (A.t U8.byte n) (n*2 + 160)

val sha2_512: #n:nat
  -> msg: A.t U8.byte n
  -> cost (A.t U8.byte n) (n*7 + 710)

val sha3_256: #n:nat
  -> msg: A.t U8.byte n
  -> cost (A.t U8.byte n) (n*6 + 1050)

val sha3_512: #n:nat
  -> msg: A.t U8.byte n
  -> cost (A.t U8.byte n) (n*8 + 800)

val sign: #n:nat
  -> msg: A.t U8.byte n
  -> key
  -> signature

val verify: #n:nat
  -> msg: A.t U8.byte n
  -> signature
  -> pubkey: key
  -> bool
