module Zen.Asset

module U32 = FStar.UInt32
module S = Zen.String

open Zen.Cost
open Zen.Types
open Zen.Types.Extracted

val zeroHash:hash
val zenAsset:asset

val getDefault: 
    contractHash
    -> asset `cost` 64
    
val fromString:
   contractHash
   -> s:string { S.length s <= 29 }  
   -> asset `cost` 64

val fromInt:
   contractHash
   -> U32.t
   -> asset `cost` 64