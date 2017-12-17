module Zen.Sha3.Realized

open Zen.Cost
open Zen.Types.Extracted

module M = FStar.Mul

val hash256: #n:nat
  -> data n
  -> cost (option hash) M.(n*384 + 1050)
