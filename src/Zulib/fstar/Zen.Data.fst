module Zen.Data

open Zen.Base
open Zen.Cost
open Zen.Types
module OT = Zen.OptionT
module A = Zen.Array
//module I8  = FStar.Int8
//module I32 = FStar.Int32
module I64 = FStar.Int64
module U8  = FStar.UInt8
module U32 = FStar.UInt32
module U64 = FStar.UInt64
module OT = Zen.OptionT
module Dict = Zen.Dictionary

val tryI64 : data -> option I64.t `cost` 2
let tryI64 = function | I64 x -> OT.incSome 2 x
                      | _ -> OT.incNone 2

val tryByte : data -> option U8.t `cost` 2
let tryByte = function | Byte b -> OT.incSome 2 b
                       | _ -> OT.incNone 2

val tryByteArray : data -> option (A.t U8.t) `cost` 2
let tryByteArray = function | ByteArray a -> OT.incSome 2 a
                            | _ -> OT.incNone 2

val tryU32 : data -> option U32.t `cost` 2
let tryU32 = function | U32 x -> OT.incSome 2 x
                      | _ -> OT.incNone 2

val tryU64 : data -> option U64.t `cost` 2
let tryU64 = function | U64 x -> OT.incSome 2 x
                      | _ -> OT.incNone 2

val tryString : data -> option string `cost` 2
let tryString = function | String s -> OT.incSome 2 s
                         | _ -> OT.incNone 2

val tryHash : data -> option hash `cost` 2
let tryHash = function | Hash h -> OT.incSome 2 h
                       | _ -> OT.incNone 2

val tryLock : data -> option lock `cost` 2
let tryLock = function | Lock l -> OT.incSome 2 l
                       | _ -> OT.incNone 2

val trySignature: data -> option signature `cost` 2
let trySignature = function | Signature s -> OT.incSome 2 s
                            | _ -> OT.incNone 2

val tryPublicKey: data -> option publicKey `cost` 2
let tryPublicKey = function | PublicKey pk -> OT.incSome 2 pk
                            | _ -> OT.incNone 2

val tryCollection: data -> option dataCollection `cost` 2
let tryCollection = function | Collection c -> OT.incSome 2 c
                             | _ -> OT.incNone 2

val tryArray: dataCollection -> option (A.t data) `cost` 2
let tryArray = function | Array a -> OT.incSome 2 a
                        | _ -> OT.incNone 2

val tryDict: dataCollection -> option (Dict.t data) `cost` 2
let tryDict = function | Dict d -> OT.incSome 2 d
                       | _ -> OT.incNone 2

val tryList: dataCollection -> option (list data) `cost` 2
let tryList = function | List l -> OT.incSome 2 l
                       | _ -> OT.incNone 2


val (>?=)(#a #b:Type)(#m #n:nat):
  cost (option a) m
  -> (a -> cost (option b) n)
  -> cost (option b) (m+n)
let (>?=) = OT.bind

val (>?>)(#a #b #c:Type)(#m #n:nat):
  f:(a -> cost (option b) m)
  -> g:(b -> cost (option c) n)
  -> a -> cost (option c) (m+n)
let (>?>) = let open OT in (>=>)

val (>!=)(#a #b:Type)(#n:nat):
  (option a) -> (a -> cost (option b) n) -> cost (option b) n
let (>!=) #_ #_ #_ = OT.liftOpt >> OT.bind
