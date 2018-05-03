module Consensus.Asset

open Types
open Hash
open Infrastructure
open FsBech32

let Zen = Asset ((ContractId (Version0, Hash.zero)), Hash.zero)

[<Literal>]
let ZeroSubTypeLength = 36

[<Literal>]
let SubTypeLength = 68

let toString (asset:Asset) = asset.ToString()

let fromString b16 =
    if b16 = "00" then
        Some Zen
    else
        match Base16.decode b16 with
        | None -> None
        | Some bytes ->
            match Array.length bytes with
            | ZeroSubTypeLength ->
                let versionBytes,assetTypeBytes = Array.splitAt 4 bytes
                let version = BigEndianBitConverter.toUint32 versionBytes
                let assetType = Hash.Hash assetTypeBytes

                Some <| Asset (ContractId (version,assetType),Hash.zero)
            | SubTypeLength ->
                let versionBytes,bytes = Array.splitAt 4 bytes
                let assetTypeBytes,subTypeBytes = Array.splitAt 32 bytes

                let version = BigEndianBitConverter.toUint32 versionBytes
                let assetType = Hash.Hash assetTypeBytes
                let subType = Hash.Hash subTypeBytes

                Some <| Asset (ContractId (version,assetType),subType)
            | _ -> None


let defaultOf contractId = Asset(contractId,Hash.zero)
