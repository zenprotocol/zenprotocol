module Zen.Hash.Sha3

open Zen.Base
open Zen.Cost
open Zen.Types

module A = Zen.Array
module I64 = FStar.Int64
module U8  = FStar.UInt8
module U32 = FStar.UInt32
module U64 = FStar.UInt64

assume type t

val empty:t

val updateHash:
  hash ->
  t ->
  t `cost` (32 * 6)

val updateAsset:
  asset ->
  t ->
  t `cost` (64 * 6)

val updateOutpoint:
  outpoint ->
  t ->
  t `cost` (36 * 6)

val updateByte:
  U8.t ->
  t ->
  t `cost` 6

val updateU32:
  U32.t ->
  t ->
  t `cost` (6 * 4)

val updateU64:
  U64.t ->
  t ->
  t `cost` (6 * 8)

val updateI64:
  I64.t ->
  t ->
  t `cost` (6 * 8)

val updateString:
  s:string ->
  t ->
  t `cost` (6 * FStar.String.length s)

val updateByteArray(#n:nat):
  U8.t `A.indexed` n ->
  t ->
  t `cost` (6 * n)

val finalize:
  t ->
  hash `cost` 20
