module Zen.ContractId

open FsBech32
open Zen.Types.Extracted
open System

module Cost = Zen.Cost.Realized

let private decodeB16Bytes =
    System.Text.Encoding.ASCII.GetString >> Base16.decode

let bytesLength = 4 + 32

let fromString (value : Prims.string) : Cost.t<contractId FStar.Pervasives.Native.option, unit> =
    lazy (
        match decodeB16Bytes value with
        | Some bytes ->
            if Seq.length bytes <> bytesLength then failwith "invalid bytes length"
            let toUint32 (bytes:byte array) =
                let bytes = if BitConverter.IsLittleEndian then Array.rev bytes else bytes
                BitConverter.ToUInt32 (bytes, 0)

            let version, cHash = toUint32 bytes.[0..3], bytes.[4..]

            FStar.Pervasives.Native.Some (version, cHash)
        | None ->
            FStar.Pervasives.Native.None
    )
    |> Cost.C
