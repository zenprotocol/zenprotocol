module Address 

open Consensus.Hash
open Consensus.ChainParameters
open FsBech32

[<Literal>]
let AddressVersion = 0uy

type Address =
    | PK of Hash
    | Contract of Hash

let private getHash =
    function
    | PK hash -> hash
    | Contract hash -> hash

let private getChainType =
    function
    | Main -> 'z'
    | Test -> 't'

let private getAddressType =
    function
    | PK _ -> 'p'
    | Contract _ -> 'c'

let private getAddress hash =
    function 
    | 'p' -> Ok (PK hash)
    | 'c' -> Ok (Contract hash)
    | _ -> Error "invaid address type"

let encode chain address =
    let (Hash.Hash bytes) = getHash address
    let hrp = System.String.Concat(Array.ofList([ getChainType chain; getAddressType address ]))
    let words = Bech32.toWords bytes
    let data = Array.append [|AddressVersion|] words
    Bech32.encode hrp data
 
let private decode chain address =
    match Bech32.decode address with 
    | None -> 
        Error "could not decode address"
    | Some (hrp, words) ->
        let hrp = Seq.toArray hrp
        if Seq.length hrp <> 2 then 
            Error "invalid HRP"
        else if hrp.[0] <> getChainType chain then
            Error "invalid chain"
        else if Array.isEmpty words then
            Error "missing address data"
        else if words.[0] <> AddressVersion then 
            Error "address version mismatch"
        else 
            match Bech32.fromWords words.[1..] with
            | None -> 
                Error "invalid address"
            | Some data -> 
                if Array.length data = 32 then 
                    getAddress (Hash data) hrp.[1]
                else 
                    Error "invalid address data"

let decodeContract chain address  =
    decode chain address
    |> Result.bind (function 
        | Contract hash ->
            Ok hash
        | _ ->
            Error "address type mismatch, Contract expected")

let decodePK chain address =
    decode chain address
    |> Result.bind (function 
        | PK hash ->
            Ok hash
        | _ ->
            Error "address type mismatch, Public Key expected")