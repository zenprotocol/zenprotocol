module Zen.Asset

open FStar.Pervasives
open Zen.Types.Extracted
open System
open System.Text
open FStar.UInt32
open FsBech32
open FStar.Pervasives

module Cost = Zen.Cost.Realized
module ContractId = Zen.ContractId

let private bom = [| 0xEFuy; 0xBBuy; 0xBFuy |]

let private filler len =
    32 - len
    |> Array.zeroCreate

let zeroHash = Array.zeroCreate 32

let zenAsset : asset = 0ul, zeroHash, zeroHash

let private decodeB16Bytes =
    System.Text.Encoding.ASCII.GetString >> Base16.decode

let encodedBytesLength = 2 * ContractId.bytesLength

let getDefault ((version,cHash) : contractId) : Cost.t<asset, unit> =
    lazy (version, cHash, zeroHash)
    |> Cost.C

let fromSubtypeString ((version,cHash) : contractId) (value : Prims.string) : Cost.t<asset, unit> =
    lazy (
        let n = Array.length value
        let bytes =
            (filler (n + 3))
            |> Array.append value
            |> Array.append bom
        version, cHash, bytes
    )
    |> Cost.C

let fromSubtypeInt ((version,cHash) : contractId) (value : uint32) : Cost.t<asset, unit> =
    lazy (
        let bytes = BitConverter.GetBytes value
        let bytes =
            if BitConverter.IsLittleEndian
            then Array.rev bytes
            else bytes
        let bytes =
            bytes
            |> Array.append (filler (Array.length bytes))
        version, cHash, bytes
    )
    |> Cost.C

let fromString (value : Prims.string) : Cost.t<asset Native.option, unit> =
    lazy (
        if value = [| 0uy; 0uy |] then
            Native.Some (0u, zeroHash, zeroHash)
        else
          let contractId = ContractId.fromString value.[0..encodedBytesLength-1] |> Cost.__force
          match contractId with
          | Native.Some (ver, cHash) ->
              let subType =
                  if Array.length value = encodedBytesLength then
                      Some zeroHash
                  else
                      match decodeB16Bytes value.[encodedBytesLength..] with
                      | Some subType -> Some subType
                      | None -> None
              match subType with
              | Some subType ->
                  Native.Some (ver, cHash, subType)
              | None ->
                  Native.None
          | Native.None ->
              Native.None
    )
    |> Cost.C
