module Zen.Hash.Sha3

open System
open Org.BouncyCastle.Crypto.Digests
open Zen.Types.Extracted

module Cost = Zen.Cost.Realized
module I64 = FStar.Int64
module U8  = FStar.UInt8
module U32 = FStar.UInt32
module U64 = FStar.UInt64
module S = FStar.String

type t = Sha3Digest

let empty = new Sha3Digest(256)

let private clone (sha3:t) = new Sha3Digest(sha3)

let private U32ToBytes (value:uint32) =
    let bytes = BitConverter.GetBytes value

    if System.BitConverter.IsLittleEndian then
        Array.rev bytes
    else
        bytes

let updateHash (h:hash) (sha3:t): Cost.t<t, unit> =
    lazy (
        let sha3 = clone sha3
        sha3.BlockUpdate(h, 0, 32)
        sha3
    )
    |> Cost.C

let updateAsset (asset:asset) (sha3:t): Cost.t<t, unit> =
    lazy (
        let sha3 = clone sha3
        let chash,assetType = asset

        sha3.BlockUpdate(chash, 0, 32)
        sha3.BlockUpdate(assetType, 0, 32)
        sha3
    )
    |> Cost.C

let updateOutpoint (outpoint:outpoint) (sha3:t): Cost.t<t, unit> =
    lazy (
        let sha3 = clone sha3
        sha3.BlockUpdate(outpoint.txHash, 0, 32)
        sha3.BlockUpdate(U32ToBytes outpoint.index, 0, 4)
        sha3
    )
    |> Cost.C

let updateByte (b:U8.t) (sha3:t): Cost.t<t, unit> =
    lazy (
        let sha3 = clone sha3
        sha3.BlockUpdate([|b|], 0, 1)
        sha3
    )
    |> Cost.C

let updateU32 (value:U32.t) (sha3:t): Cost.t<t, unit> =
    lazy (
        let sha3 = clone sha3
        sha3.BlockUpdate(U32ToBytes value, 0, 4)
        sha3
    )
    |> Cost.C

let updateU64 (value:U64.t) (sha3:t): Cost.t<t, unit> =
    lazy (
        let sha3 = clone sha3

        let bytes =
            let bytes = BitConverter.GetBytes value

            if System.BitConverter.IsLittleEndian then
                Array.rev bytes
            else
                bytes

        sha3.BlockUpdate(bytes, 0, 8)
        sha3
    )
    |> Cost.C

let updateI64 (value:I64.t) (sha3:t): Cost.t<t, unit> =
    lazy (
        let sha3 = clone sha3

        let bytes =
            let bytes = BitConverter.GetBytes value

            if System.BitConverter.IsLittleEndian then
                Array.rev bytes
            else
                bytes

        sha3.BlockUpdate(bytes, 0, 8)
        sha3
    )
    |> Cost.C

let updateString (s:S.t) (sha3:t): Cost.t<t, unit> =
    lazy (
        let sha3 = clone sha3
        sha3.BlockUpdate(s, 0, Array.length s)
        sha3
    )
    |> Cost.C

let updateByteArray (bs:U8.t array) (sha3:t): Cost.t<t, unit> =
    lazy (
        let sha3 = clone sha3
        sha3.BlockUpdate(bs, 0, Array.length bs)
        sha3
    )
    |> Cost.C

let finalize (sha3:t): Cost.t<hash, unit> =
    lazy (
        let sha3 = clone sha3
        let hash = Array.zeroCreate 32

        sha3.DoFinal(hash, 0) |> ignore
        hash
    )
    |> Cost.C
