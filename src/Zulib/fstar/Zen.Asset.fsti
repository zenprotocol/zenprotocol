module Zen.Asset

module U32 = FStar.UInt32
module S = FStar.String

open Zen.Cost
open Zen.Types
open Zen.Types.Extracted
open FStar.Pervasives.Native

val zeroHash:hash
val zenAsset:asset

val getDefault:
    contractId
    -> asset `cost` 64

val fromSubtypeString:
    contractId
    -> s:string { S.length s <= 29 }
    -> asset `cost` 64

val fromSubtypeInt:
    contractId
    -> U32.t
    -> asset `cost` 64

val fromString:
    string
    -> option asset `cost` 64
