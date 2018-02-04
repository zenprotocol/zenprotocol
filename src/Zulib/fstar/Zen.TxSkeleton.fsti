module Zen.TxSkeleton

open Zen.Cost
open Zen.Types

module M = FStar.Mul
module V = Zen.Vector
module U64 = FStar.UInt64

val getAvailableTokens: asset -> txSkeleton -> U64.t `cost` 64

val addInput: pointedOutput -> txSkeleton -> txSkeleton `cost` 64

val addInputs(#n:nat):
  pointedOutput `V.t` n
  -> txSkeleton
  -> txSkeleton `cost` M.(64 * n + 64)

val lockToContract:
  asset
  -> U64.t
  -> contractHash
  -> txSkeleton
  -> txSkeleton `cost` 64

val lockToPubKey:
  asset
  -> U64.t
  -> pkHash:hash
  -> txSkeleton
  -> txSkeleton `cost` 64

val lockToAddress:
  asset
  -> U64.t
  -> address:lock
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
  
val fromWallet(#n:nat):
  asset ->
  amount:U64.t ->
  contractHash -> 
  wallet n ->  
  txSkeleton ->
  option txSkeleton `cost` M.(n * 128 + 192)   

val isValid: txSkeleton -> bool `cost` 64
