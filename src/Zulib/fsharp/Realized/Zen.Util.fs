module Zen.Util

open Zen.Types.Extracted

module A = Zen.Array.Realized

(* TODO: add cost, array's size? *)
let hashFromBase64 (b64 : Prims.string) : A.array<byte, unit> =
    System.Text.Encoding.ASCII.GetString b64
    |> System.Convert.FromBase64String

let contractIdFromBase64 (b64: Prims.string) : contractId =
    let bytes =
        System.Text.Encoding.ASCII.GetString b64
        |> System.Convert.FromBase64String

    let versionBytes,hashBytes = Array.splitAt 4 bytes

    let version =
        if System.BitConverter.IsLittleEndian then Array.rev versionBytes else versionBytes
        |> fun bytes -> System.BitConverter.ToUInt32 (bytes,0)

    version,hashBytes

let debug (x:'A) : 'A =
    printfn "%A" x
    x
