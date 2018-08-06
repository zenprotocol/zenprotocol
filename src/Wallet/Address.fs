module Wallet.Address

open Consensus
open Consensus.Hash
open Consensus.Chain
open Consensus.Types
open FsBech32
open Infrastructure

[<Literal>]
let AddressVersion = 0uy

type AddressType =
    | PKAddressType
    | ContractAddressType

type Address =
    | PK of Hash
    | Contract of ContractId

[<Literal>]
let private contractAddressIdentifier = "c"

[<Literal>]
let private mainChainIdentifier = "zen"

[<Literal>]
let private testChainIdentifier = "tzn"

let private getChainType =
    function
    | Main -> mainChainIdentifier
    | Local
    | Test -> testChainIdentifier

let encode chain address =
    let bytes =
        match address with
        | PK hash -> Hash.bytes hash
        | Contract contractId -> ContractId.toBytes contractId

    let getAddressType =
        function
        | PK _ -> ""
        | Contract _ -> contractAddressIdentifier

    let hrp = String.concat "" [ getAddressType address; getChainType chain ]
    let words = Bech32.toWords bytes
    let data = Array.append [|AddressVersion|] words
    Bech32.encode hrp data

type State =
    | Invalid of string
    | DecodeHrp of string * expectedChain:Chain * expectedAddressType:AddressType * byte[]
    | DecodeHrpNext of AddressType * expectedChain:Chain * string * byte[]
    | DecodeData of AddressType * byte[]
    | Valid of Address

let private decodeHrp hrp expectedChain expectedAddressType data =
    let hrpLen = String.length hrp

    if hrpLen = 3 &&
       expectedAddressType = PKAddressType then
        DecodeHrpNext (PKAddressType, expectedChain, hrp, data)
    else if hrpLen = 4 &&
            hrp.[0] = contractAddressIdentifier.[0] &&
            expectedAddressType = ContractAddressType then
        DecodeHrpNext (ContractAddressType, expectedChain, hrp.[1..], data)
    else
        Invalid "invalid HRP"

let private decodeHrpNext addressType expectedChain hrp data =
    if hrp = mainChainIdentifier &&
       expectedChain = Main then
        DecodeData (addressType, data)
    else if hrp = testChainIdentifier &&
            (expectedChain = Test || expectedChain = Local) then
        DecodeData (addressType, data)
    else
        Invalid "invalid HRP"

let private decodeData addressType (data:byte[]) =
    if data.[0] <> AddressVersion then
        Invalid "address version mismatch"
    else
        match Bech32.fromWords data.[1..] with
        | None ->
            Invalid "invalid address"
        | Some data ->
            match addressType with
            | PKAddressType ->
                Hash.fromBytes data
                |> Option.map PK
            | ContractAddressType ->
                ContractId.fromBytes data
                |> Option.map Contract
            |> function
            | Some address -> Valid address
            | None -> Invalid "invalid address data"

let rec private decodeNext state =
    match state with
    | Invalid err -> Invalid err
    | DecodeHrp (hrp, expectedChain, expectedAddressType, data) ->
        decodeHrp hrp expectedChain expectedAddressType data
        |> decodeNext
    | DecodeHrpNext (addressType, expectedChain, hrp, data) ->
        decodeHrpNext addressType expectedChain hrp data
        |> decodeNext
    | DecodeData (addressType, data) ->
        decodeData addressType data
        |> decodeNext
    | Valid address -> Valid address

let private decode chain addressType bytes =
    match Bech32.decode bytes with
    | None ->
        Error "could not decode address"
    | Some (hrp, data) ->
        match decodeNext (DecodeHrp (hrp, chain, addressType, data)) with
        | Valid address -> Ok address
        | Invalid error -> Error error
        | _ -> failwith "unexpected"

let decodeContract chain address =
    decode chain ContractAddressType address
    |> Result.bind (function
        | Contract contractId ->
            Ok contractId
        | _ ->
            Error "address type mismatch, Contract expected")

let decodePK chain address =
    decode chain PKAddressType address
    |> Result.bind (function
        | PK hash ->
            Ok hash
        | _ ->
            Error "address type mismatch, Public Key expected")

let decodeAny chain (address:string) =
    let prefix = getChainType chain

    if address.StartsWith(prefix) then
        decodePK chain address |> Result.map PK
    else
        decodeContract chain address |> Result.map Contract