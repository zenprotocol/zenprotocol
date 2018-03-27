module Zen.Data

open Zen.Cost
open Zen.Types
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

val tryI64Array : data -> option (l:nat & I64.t `A.t` l) `cost` 2
let tryI64Array = function | I64Array a -> OT.incSome 2 a
                           | _ -> OT.incNone 2

val tryByte : data -> option U8.t `cost` 2
let tryByte = function | Byte b -> OT.incSome 2 b
                       | _ -> OT.incNone 2

val tryByteArray : data -> option (l:nat & U8.t `A.t` l) `cost` 2
let tryByteArray = function | ByteArray a -> OT.incSome 2 a
                            | _ -> OT.incNone 2

val tryU32 : data -> option U32.t `cost` 2
let tryU32 = function | U32 x -> OT.incSome 2 x
                      | _ -> OT.incNone 2

val tryU32Array : data -> option (l:nat & U32.t `A.t` l) `cost` 2
let tryU32Array = function | U32Array a -> OT.incSome 2 a
                           | _ -> OT.incNone 2

val tryU64 : data -> option U64.t `cost` 2
let tryU64 = function | U64 x -> OT.incSome 2 x
                      | _ -> OT.incNone 2

val tryU64Array : data -> option (l:nat & U64.t `A.t` l) `cost` 2
let tryU64Array = function | U64Array a -> OT.incSome 2 a
                         | _ -> OT.incNone 2

val tryString : data -> option string `cost` 2
let tryString = function | String s -> OT.incSome 2 s
                         | _ -> OT.incNone 2

val tryStringArray : data -> option (l:nat & string `A.t` l) `cost` 2
let tryStringArray = function | StringArray a -> OT.incSome 2 a
                              | _ -> OT.incNone 2

val tryHash : data -> option hash `cost` 2
let tryHash = function | Hash h -> OT.incSome 2 h
                       | _ -> OT.incNone 2

val tryHashArray : data -> option (l:nat & hash `A.t` l) `cost` 2
let tryHashArray = function | HashArray a -> OT.incSome 2 a
                            | _ -> OT.incNone 2

val tryLock : data -> option lock `cost` 2
let tryLock = function | Lock l -> OT.incSome 2 l
                       | _ -> OT.incNone 2

val tryLockArray : data -> option (l:nat & lock `A.t` l) `cost` 2
let tryLockArray = function | LockArray a -> OT.incSome 2 a
                            | _ -> OT.incNone 2

val tryTuple : data -> option (data**data) `cost` 2
let tryTuple = function | Tuple l -> OT.incSome 2 l
                        | _ -> OT.incNone 2

val tryDict : data -> option dataDict `cost` 2
let tryDict = function | Dict d -> OT.incSome 2 d
                       | _ -> OT.incNone 2

val tryFindI64 : string -> dataDict -> option I64.t `cost` 66
let tryFindI64 s (DataDict d) = Dict.tryFind s d `OT.bind` tryI64

val tryFindI64Array : string -> dataDict -> option (l:nat & I64.t `A.t` l) `cost` 66
let tryFindI64Array s (DataDict d) = Dict.tryFind s d `OT.bind` tryI64Array

val tryFindByte : string -> dataDict -> option U8.t `cost` 66
let tryFindByte s (DataDict d) = Dict.tryFind s d `OT.bind` tryByte

val tryFindByteArray : string -> dataDict -> option (l:nat & U8.t `A.t` l) `cost` 66
let tryFindByteArray s (DataDict d) = Dict.tryFind s d `OT.bind` tryByteArray

val tryFindU32 : string -> dataDict -> option U32.t `cost` 66
let tryFindU32 s (DataDict d) = Dict.tryFind s d `OT.bind` tryU32

val tryFindU32Array : string -> dataDict -> option (l:nat & U32.t `A.t` l) `cost` 66
let tryFindU32Array s (DataDict d) = Dict.tryFind s d `OT.bind` tryU32Array

val tryFindU64 : string -> dataDict -> option U64.t `cost` 66
let tryFindU64 s (DataDict d) = Dict.tryFind s d `OT.bind` tryU64

val tryFindU64Array : string -> dataDict -> option (l:nat & U64.t `A.t` l) `cost` 66
let tryFindU64Array s (DataDict d) = Dict.tryFind s d `OT.bind` tryU64Array

val tryFindString : string -> dataDict -> option string `cost` 66
let tryFindString s (DataDict d) = Dict.tryFind s d `OT.bind` tryString

val tryFindStringArray : string -> dataDict -> option (l:nat & string `A.t` l) `cost` 66
let tryFindStringArray s (DataDict d) = Dict.tryFind s d `OT.bind` tryStringArray

val tryFindHash : string -> dataDict -> option hash `cost` 66
let tryFindHash s (DataDict d) = Dict.tryFind s d `OT.bind` tryHash

val tryFindHashArray : string -> dataDict -> option (l:nat & hash `A.t` l) `cost` 66
let tryFindHashArray s (DataDict d) = Dict.tryFind s d `OT.bind` tryHashArray

val tryFindLock : string -> dataDict -> option lock `cost` 66
let tryFindLock s (DataDict d) = Dict.tryFind s d `OT.bind` tryLock

val tryFindLockArray : string -> dataDict -> option (l:nat & lock `A.t` l) `cost` 66
let tryFindLockArray s (DataDict d) = Dict.tryFind s d `OT.bind` tryLockArray

val tryFindTuple : string -> dataDict -> option (data**data) `cost` 66
let tryFindTuple s (DataDict d) = Dict.tryFind s d `OT.bind` tryTuple

val tryFindDict : string -> dataDict -> option dataDict `cost` 66
let tryFindDict s (DataDict d) = Dict.tryFind s d `OT.bind` tryDict
