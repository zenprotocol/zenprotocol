module Address 

open Consensus
open Consensus.Crypto

let getPublicKeyAddress pkHash =         
    let bytes = Hash.bytes pkHash
        
    System.Convert.ToBase64String(bytes)
 
let getPublicKeyHash (address:string) =
    let bytes = System.Convert.FromBase64String(address)
    
    Hash.fromBytes bytes