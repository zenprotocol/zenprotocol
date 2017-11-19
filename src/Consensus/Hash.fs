module Consensus.Hash

open Org.BouncyCastle.Crypto.Digests
open FsBech32

type Hash = Hash of byte[]

let zero = 
    Hash (Array.create 32 0uy)

let compute bytes = 
    let hash = Array.zeroCreate 32
    let sha3 = new Sha3Digest(256)
    sha3.BlockUpdate(bytes,0,Array.length bytes)
    sha3.DoFinal(hash, 0) |> ignore
    Hash hash

let bytes (Hash hash) = hash

let fromBytes bytes = 
    match Array.length bytes with
    | 32 -> Some (Hash bytes)
    | _ -> None
    
let toString (Hash hash) =        
    Base16.encode hash
    
let fromString encoded = 
    match Base16.decode encoded with
    | None -> None
    | Some decoded -> Hash decoded |> Some     
    