module Zen.Types.Data

open Zen.Dictionary
open Zen.Types.Extracted

module A = Zen.Array
//module I8  = FStar.Int8
//module I32 = FStar.Int32
module I64 = FStar.Int64
module U8  = FStar.UInt8
module U32 = FStar.UInt32
module U64 = FStar.UInt64

type data =
    //| I8  of I8.t
    //| I32 of I32.t
    | I64 of I64.t
    | I64Array: (l:nat & I64.t `A.t` l) -> data
    | Byte of U8.t
    | ByteArray: (l:nat & U8.t `A.t` l) -> data
    | U32 of U32.t
    | U32Array: (l:nat & U32.t `A.t` l) -> data
    | U64 of U64.t
    | U64Array: (l:nat & U64.t `A.t` l) -> data
    | String of string
    | StringArray: (l:nat & string `A.t` l) -> data
    | Hash of hash
    | HashArray: (l:nat & hash `A.t` l) -> data
    | Lock of lock
    | LockArray: (l:nat & lock `A.t` l) -> data
    | Tuple of (data ** data)
    | Dict of dataDict

and dataDict =
    | DataDict of dictionary data
