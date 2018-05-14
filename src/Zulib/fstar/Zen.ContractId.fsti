module Zen.ContractId

module U32 = FStar.UInt32
module S = FStar.String

open Zen.Cost
open Zen.Types
open Zen.Types.Extracted
open FStar.Pervasives.Native

val fromString:
    s:string { S.length s = 72 }
    -> option contractId `cost` 64
