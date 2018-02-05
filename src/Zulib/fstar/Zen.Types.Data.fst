module Zen.Types.Data

open Zen.Types.Extracted
open Zen.Cost

module A = Zen.Array
module OT = Zen.OptionT
//module I8  = FStar.Int8
//module I32 = FStar.Int32
module I64 = FStar.UInt64
module U8  = FStar.UInt8
module U32 = FStar.UInt32
module U64 = FStar.UInt64

type data =
    //| I8  of I8.t
    //| I32 of I32.t
    | I64 of I64.t
    | I64Array: #l:nat -> I64.t `A.t` l -> data
    | Byte of U8.t
    | ByteArray: #l:nat -> U8.t `A.t` l -> data
    | U32 of U32.t
    | U32Array: #l:nat -> U32.t `A.t` l -> data
    | U64 of U64.t
    | U64Array: #l:nat -> U64.t `A.t` l -> data
    | String of string
    | StringArray: #l:nat -> U64.t `A.t` l -> data
    | Hash of hash
    | HashArray: #l:nat -> U64.t `A.t` l -> data
    | Lock of lock
    | LockArray: #l:nat -> U64.t `A.t` l -> data
    | Tuple of (data ** data)

val getI64 : data -> option I64.t `cost` 2
let getI64 = function | I64 x -> OT.incSome 2 x
                      | _ -> OT.incNone 2

val getByte : data -> option U8.t `cost` 2
let getByte = function | Byte b -> OT.incSome 2 b
                       | _ -> OT.incNone 2

val getU32 : data -> option U32.t `cost` 2
let getU32 = function | U32 x -> OT.incSome 2 x
                      | _ -> OT.incNone 2

val getU64 : data -> option U64.t `cost` 2
let getU64 = function | U64 x -> OT.incSome 2 x
                      | _ -> OT.incNone 2

val getString : data -> option string `cost` 2
let getString = function | String s -> OT.incSome 2 s
                         | _ -> OT.incNone 2

val getHash : data -> option hash `cost` 2
let getHash = function | Hash h -> OT.incSome 2 h
                       | _ -> OT.incNone 2

val getLock : data -> option lock `cost` 2
let getLock = function | Lock l -> OT.incSome 2 l
                       | _ -> OT.incNone 2
