module Consensus.Hash

open Org.BouncyCastle.Crypto.Digests
open FsBech32

[<Literal>]
let Length = 32

[<StructuredFormatDisplay("{AsString}")>]
type Hash = Hash of byte[] with
    override x.ToString() =
        let (Hash bytes) = x
        Base16.encode bytes
    member x.AsString = x.ToString()

let zero =
    Hash (Array.create Length 0uy)

let compute bytes =
    let hash = Array.zeroCreate Length
    let sha3 = new Sha3Digest(256)
    sha3.BlockUpdate(bytes,0,Array.length bytes)
    sha3.DoFinal(hash, 0) |> ignore
    Hash hash

let computeMultiple (bytes: byte array seq) =
    let hash = Array.zeroCreate Length
    let sha3 = new Sha3Digest(256)

    Seq.iter (fun bytes -> sha3.BlockUpdate(bytes,0,Array.length bytes)) bytes
    sha3.DoFinal(hash, 0) |> ignore
    Hash hash

let bytes (Hash hash) = hash

let fromBytes bytes =
    match Array.length bytes with
    | Length -> Some (Hash bytes)
    | _ -> None

let toString h =
    h.ToString()

let fromString encoded =
    match Base16.decode encoded with
    | None -> None
    | Some decoded -> Hash decoded |> Some

let isValid (Hash hash) =
    Array.length hash = 32

let toBigInt (h:Hash) =
    h |> bytes
            |> Array.append [|0uy|] // adding zero as the MSB to avoid rare case of negative number
            |> Array.rev            // We have to reverse, as we use big-endian and bigint using little
            |> bigint

let fromBigInt (b:bigint) =
    let bs = b.ToByteArray()
    let h =
        match bs.Length with
        | n when n <= 32 ->
            let ar = Array.zeroCreate (32 - n)
            Array.rev (Array.append ar bs)
        | 33 ->
            if bs.[0] <> 0uy then failwith "Negative difficulty target"
            else Array.rev bs.[1..]
        | _ -> failwith "Difficulty target out of range"
    Hash h
