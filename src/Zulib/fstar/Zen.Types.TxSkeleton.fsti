module Zen.Types.TxSkeleton

open FStar.Mul
open Zen.Cost
open Zen.Types.Extracted

module V = Zen.Vector
module U64 = FStar.UInt64

assume type txSkeleton
assume TxSkeleton_hasEq: hasEq txSkeleton

val getAvailableTokens: asset -> txSkeleton -> U64.t `cost` 64

val addInput: pointedOutput -> txSkeleton -> txSkeleton `cost` 64

val addInputs(#n:nat):
  pointedOutput `V.t` n
  -> txSkeleton
  -> txSkeleton `cost` (64 * n + 64)

val lockToContract:
  spend
  -> contractHash
  -> txSkeleton
  -> txSkeleton `cost` 64

val lockToPubKey:
  spend
  -> pkHash:hash
  -> txSkeleton
  -> txSkeleton `cost` 64

val lockToAddress:
  spend
  -> address:hash
  -> txSkeleton
  -> txSkeleton `cost` 64

val addChangeOutput:
  asset
  -> contractHash
  -> txSkeleton
  -> txSkeleton `cost` 64

val mint:
  amount:U64.t
  -> contractHash
  -> txSkeleton
  -> txSkeleton `cost` 64

val destroy:
  amount:U64.t
  -> contractHash
  -> txSkeleton
  -> txSkeleton `cost` 64
