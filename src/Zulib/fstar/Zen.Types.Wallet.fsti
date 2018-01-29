module Zen.Types.Wallet

open Zen.Types.Extracted
open Zen.Types.TxSkeleton
open Zen.Option
open Zen.Cost

module U64 = FStar.UInt64
module V = Zen.Vector
module M = FStar.Mul

type wallet (n:nat) = pointedOutput `V.t` n

val getInputs: #n:nat -> asset -> amount:U64.t -> wallet n -> txSkeleton -> option txSkeleton `cost` M.(n * 128 + 128)

val stripInputs: #n:nat -> txSkeleton -> wallet n -> (n1:nat{n1<= n} & wallet n1) `cost` M.(n * 32)

  
