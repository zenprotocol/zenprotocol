module Address 

open Consensus
open Consensus.Crypto
open Consensus.ChainParameters
open FsBech32

[<Literal>]
let AddressVersion = 0uy

let getPublicKeyAddress pkHash chain =         
    let bytes = Hash.bytes pkHash
    
    let hrp = 
        match chain with 
        | Main -> "zp"
        | Test -> "tz"

    // TODO: when we have serialization in place we should encode
    // the lock type into the address as well        
    
    let words = Bech32.toWords (Hash.bytes pkHash)
    
    let data = Array.append [|AddressVersion|] words
    
    Bech32.encode hrp data                
 
let getPublicKeyHash (address:string) =
    match Bech32.decode address with 
    | None -> None
    | Some (hrp, words) ->         
        if Array.length words = 0 then
            None
        else                 
            let version = words.[0]
            
            if version <> AddressVersion then 
                None
            else 
                match Bech32.fromWords words.[1..] with
                | None -> None 
                | Some data -> 
                    if Array.length data = 32 then Some (Hash.Hash data) else None