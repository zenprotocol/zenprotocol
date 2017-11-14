module Consensus.Hash

open Org.BouncyCastle.Crypto.Digests

type Hash = Hash of byte[]

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
    // TODO: we should use base32 part from bech32, this shouldn't include checksum and hrp
    
    System.Convert.ToBase64String (hash)