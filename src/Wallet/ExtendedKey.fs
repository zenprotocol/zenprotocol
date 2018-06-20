module Wallet.ExtendedKey

open System
open System.Text
open System.Security.Cryptography
open Infrastructure.Result
open Infrastructure.BigInteger
open Consensus
open Crypto
open NBitcoin

let private result = new ResultBuilder<string>()

[<Literal>]
let private preSeed = "Bitcoin seed"

type T =
    | ExtendedPrivateKey of ExtKey
    | ExtendedPublicKey of ExtPubKey

let getPublicKey extendedKey =
    match extendedKey with
    | ExtendedPrivateKey key ->
        key.Neuter().PubKey.Compress().ToBytes()
        |> PublicKey.deserialize
        |> ofOption "private key invalid"
    | ExtendedPublicKey key ->
        key.PubKey.Compress().ToBytes()
        |> PublicKey.deserialize
        |> ofOption "public key invalid"

let getPrivateKey extendedKey =
    match extendedKey with
    | ExtendedPrivateKey privateKey->
        privateKey.PrivateKey.ToBytes() |> SecretKey |> Ok
    | _ -> Error "underlying key mismatch"

let getKeyPair extendedKey =
    getPrivateKey extendedKey
    >>= (fun privKey ->
        SecretKey.getPublicKey privKey
        |> Option.map (fun pubKey -> privKey,pubKey)
        |> Infrastructure.Result.ofOption "invalid secretKey")

let Hardened = (+) 0x80000000
let private isHardened i = i < 0

let create (seed:byte array) =
    try
        let key = new ExtKey(seed)
        ExtendedPrivateKey key
        |> Ok
    with
        | _ -> Error "invalid seed"

let fromMnemonicPhrase (mnemonicPhrase:string) =
    try
         let mnemonicSentence = new NBitcoin.Mnemonic(mnemonicPhrase, NBitcoin.Wordlist.English)

         mnemonicSentence.DeriveSeed ()
         |> create
    with _ as ex ->
        Error ex.Message

let derive (i:int) parent =
    try
        match parent with
        | ExtendedPrivateKey key ->
            key.Derive (uint32 i)
            |> ExtendedPrivateKey
            |> Ok
        | ExtendedPublicKey key ->
            key.Derive (uint32 i)
            |> ExtendedPublicKey
            |> Ok
    with
    | _ -> Error "cannot derive"

let neuter key =
    try
        match key with
        | ExtendedPrivateKey key ->
            key.Neuter()
            |> ExtendedPublicKey
            |> Ok
        | key ->
            key
            |> Ok
    with
    | _ -> Error "cannot neuter"

let derivePath path extendedKey = result {
    let! indexes = KeyPathParser.parse path

    return! List.fold (fun extendedKey index -> extendedKey >>= derive index) (Ok extendedKey) indexes
}

let sign msg extendedKey = result {
    let! secretKey = getPrivateKey extendedKey

    return Crypto.sign secretKey msg
}