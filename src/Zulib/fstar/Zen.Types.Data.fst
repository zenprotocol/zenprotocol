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
    | I64 of I64.t
    | Byte of U8.t
    | ByteArray: A.t U8.t -> data
    | U32 of U32.t
    | U64 of U64.t
    | String of string
    | Hash of hash
    | Lock of lock
    | Signature of signature
    | PublicKey of publicKey
    | Collection of dataCollection

and dataCollection =
    | Array of A.t data
    | Dict of dictionary data
    | List of list data
