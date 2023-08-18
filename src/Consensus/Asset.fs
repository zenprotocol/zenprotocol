module Consensus.Asset

open Types
open Hash
open Infrastructure
open FsBech32

// Define a default Zen asset
let Zen = Asset ((ContractId (Version0, Hash.zero)), Hash.zero)

[<Literal>]
let ZeroSubTypeLength = 36

[<Literal>]
let SubTypeLength = 68

/// Converts an asset to its string representation.
let toString (asset: Asset) = asset.ToString()

/// Converts a string to its corresponding asset.
/// Returns None if conversion fails.
let fromString b16 =
    match b16, Base16.decode b16 with
    | "00", _ -> Some Zen
    | _, None -> None
    | _, Some bytes when Array.length bytes = ZeroSubTypeLength ->
        let versionBytes, assetTypeBytes = Array.splitAt 4 bytes
        let version = BigEndianBitConverter.toUint32 versionBytes
        let assetType = Hash.Hash assetTypeBytes
        Some (Asset (ContractId (version, assetType), Hash.zero))
    | _, Some bytes when Array.length bytes = SubTypeLength ->
        let versionBytes, bytes = Array.splitAt 4 bytes
        let assetTypeBytes, subTypeBytes = Array.splitAt 32 bytes
        let version = BigEndianBitConverter.toUint32 versionBytes
        let assetType = Hash.Hash assetTypeBytes
        let subType = Hash.Hash subTypeBytes
        Some (Asset (ContractId (version, assetType), subType))
    | _, _ -> None

/// Returns a default asset with the given contractId and a zero hash.
let defaultOf contractId = Asset(contractId, Hash.zero)

/// Extracts the contractId from an asset.
let contractId (Asset (contractId, _)) = contractId
