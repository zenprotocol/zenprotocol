module Consensus.Hash

open Org.BouncyCastle.Crypto.Digests
open FsBech32
open Infrastructure.BigInteger

[<Literal>]
let Length = 32

[<StructuredFormatDisplay("{AsString}")>]
type Hash = Hash of byte[] with
    override x.ToString() =
        let (Hash bytes) = x
        Base16.encode bytes
    member x.AsString = x.ToString()

let zero =
    Hash (Array.zeroCreate Length)

let compute bytes =
    let hash = Array.zeroCreate Length
    let sha3 = new Sha3Digest(256)
    sha3.BlockUpdate(bytes,0,Array.length bytes)
    sha3.DoFinal(hash, 0) |> ignore
    Hash hash

let computeOfHash (Hash bytes) = compute bytes

let computeMultiple (bytes: byte array seq) =
    let hash = Array.zeroCreate Length
    let sha3 = new Sha3Digest(256)

    Seq.iter (fun bytes -> sha3.BlockUpdate(bytes,0,Array.length bytes)) bytes
    sha3.DoFinal(hash, 0) |> ignore
    Hash hash

let bytes (Hash hash) = hash

let joinHashes : Hash seq -> _ =
    Seq.map bytes
    >> computeMultiple

let fromBytes bytes =
    match Array.length bytes with
    | Length -> Some (Hash bytes)
    | _ -> None

let toString (h:Hash) =
    h.ToString()

let fromString encoded =
    match Base16.decode encoded with
    | None -> Error "Could not decode hash"
    | Some decoded -> Hash decoded |> Ok

let isValid (Hash hash) =
    Array.length hash = Length

let toBigInt = bytes >> fromBytes32

let fromBigInt = toBytes32 >> Hash