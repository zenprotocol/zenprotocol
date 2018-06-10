module Address

open Consensus
open Consensus.Hash
open Consensus.Chain
open Consensus.Types
open FsBech32
open Infrastructure

[<Literal>]
let AddressVersion = 0uy

type Address =
    | PK of Hash
    | Contract of ContractId

let private getChainType =
    function
    | Main -> 'z'
    | Local
    | Test -> 't'

let private getAddressType =
    function
    | PK _ -> 'p'
    | Contract _ -> 'c'

let encode chain address =
    let bytes =
        match address with
        | PK hash -> Hash.bytes hash
        | Contract contractId -> ContractId.toBytes contractId

    let hrp = System.String.Concat(Array.ofList([ getChainType chain; getAddressType address ]))
    let words = Bech32.toWords bytes
    let data = Array.append [|AddressVersion|] words
    Bech32.encode hrp data

let decode chain address =
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
                match hrp.[1] with
                | 'p' -> Hash.fromBytes data |> Option.map PK |> Result.ofOption "invalid address data"
                | 'c' -> ContractId.fromBytes data |> Option.map Contract |> Result.ofOption "invalid address data"
                | _ -> Error "invaid address type"

let decodeContract chain address  =
    decode chain address
    |> Result.bind (function
        | Contract contractId ->
            Ok contractId
        | _ ->
            Error "address type mismatch, Contract expected")

let decodePK chain address =
    decode chain address
    |> Result.bind (function
        | PK hash ->
            Ok hash
        | _ ->
            Error "address type mismatch, Public Key expected")