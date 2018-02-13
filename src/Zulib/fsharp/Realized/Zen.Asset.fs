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

let zenAsset : asset = zeroHash, zeroHash

let getDefault (cHash : contractHash) : Cost.t<asset, unit> =
    lazy (cHash, zeroHash)
    |> Cost.C

let fromString (cHash : contractHash) (value : string) : Cost.t<asset, unit> =
    lazy (
        let bytes = Encoding.UTF8.GetBytes value
        let n = Array.length bytes
        let bytes =
            (filler (n + 3))
            |> Array.append bytes
            |> Array.append bom
        cHash, bytes
    )
    |> Cost.C

let fromInt (cHash : contractHash) (value : uint32) : Cost.t<asset, unit> =
    lazy (
        let bytes = BitConverter.GetBytes value
        let bytes =
            if System.BitConverter.IsLittleEndian
            then Array.rev bytes
            else bytes
        let bytes =
            bytes
            |> Array.append (filler (Array.length bytes)) 
        cHash, bytes
    )
    |> Cost.C