module Zen.Asset

open FStar.Pervasives
open Zen.Types.Extracted
open System
open System.Text
open FStar.UInt32

module Cost = Zen.Cost.Realized

let private bom = [| 0xEFuy; 0xBBuy; 0xBFuy |]

let private filler len =
    32 - len
    |> Array.zeroCreate

let zeroHash = Array.zeroCreate 32

let zenAsset : asset = 0ul, zeroHash, zeroHash

let getDefault ((version,cHash) : contractId) : Cost.t<asset, unit> =
    lazy (version, cHash, zeroHash)
    |> Cost.C

let fromString ((version,cHash) : contractId) (value : Prims.string) : Cost.t<asset, unit> =
    lazy (
        let n = Array.length value
        let bytes =
            (filler (n + 3))
            |> Array.append value
            |> Array.append bom
        version, cHash, bytes
    )
    |> Cost.C

let fromInt ((version,cHash) : contractId) (value : uint32) : Cost.t<asset, unit> =
    lazy (
        let bytes = BitConverter.GetBytes value
        let bytes =
            if System.BitConverter.IsLittleEndian
            then Array.rev bytes
            else bytes
        let bytes =
            bytes
            |> Array.append (filler (Array.length bytes))
        version, cHash, bytes
    )
    |> Cost.C
