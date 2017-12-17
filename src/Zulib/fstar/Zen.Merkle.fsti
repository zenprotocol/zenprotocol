module Zen.Merkle

open Zen.Cost
module A = Zen.Array
module M = FStar.Mul
module C = Zen.Crypto
module U32 = FStar.UInt32
open Zen.Types.Extracted


val rootFromAuditPath: #n:nat
  -> C.hash
  -> U32.t
  -> A.t C.hash n
  -> cost (C.hash) n

val hashData: #n:nat
-> data n
-> cost (option hash) M.(n*384 + 1050)
    
