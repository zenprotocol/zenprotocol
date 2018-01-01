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
    Hash (Array.create 32 0uy)

let compute bytes = 
    let hash = Array.zeroCreate 32
    let sha3 = new Sha3Digest(256)
    sha3.BlockUpdate(bytes,0,Array.length bytes)
    sha3.DoFinal(hash, 0) |> ignore
    Hash hash
    
let computeMultiple (bytes: byte array seq) = 
    let hash = Array.zeroCreate 32
    let sha3 = new Sha3Digest(256)
    
    Seq.iter (fun bytes -> sha3.BlockUpdate(bytes,0,Array.length bytes)) bytes
    sha3.DoFinal(hash, 0) |> ignore
    Hash hash

let bytes (Hash hash) = hash

let fromBytes bytes = 
    match Array.length bytes with
    | 32 -> Some (Hash bytes)
    | _ -> None
    
let toString h =        
    h.ToString()
    
let fromString encoded = 
    match Base16.decode encoded with
    | None -> None
    | Some decoded -> Hash decoded |> Some     

let isValid (Hash hash) =
    Array.length hash = 32