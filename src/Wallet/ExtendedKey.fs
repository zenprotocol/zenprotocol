module Wallet.ExtendedKey

open System
open System.Text
open System.Security.Cryptography
open Infrastructure.Result
open Infrastructure.BigInteger
open Consensus
open Crypto

let private result = new ResultBuilder<string>()

[<Literal>]
let private preSeed = "Bitcoin seed"

type ChainKeyArgs = {
    Private: byte[]
    Public: byte[]
}

type Key =
    | ExtendedPrivateKey of Crypto.SecretKey
    | ExtendedPublicKey of Crypto.PublicKey

type T = {
    key: Key
    depth: byte
    chainCode: byte[]
    parentFingetprint: byte[]
    index: int32
}

let getPublicKey extendedKey =
    match extendedKey.key with
    | ExtendedPrivateKey (SecretKey prvBytes) ->
        match SecretKey.getPublicKey (SecretKey prvBytes) with
        | Some pulicKey -> Ok pulicKey
        | None -> Error "private key invalid"
    | ExtendedPublicKey publicKey -> Ok publicKey

let getPrivateKey extendedKey =
    match extendedKey.key with
    | ExtendedPrivateKey privateKey-> Ok privateKey
    | _ -> Error "underlying key mismatch"

let private getIdentifier extendedKey = result {
    let! publicKey = getPublicKey extendedKey
    let sha256 = new SHA256Managed()
    let hash = sha256.ComputeHash(PublicKey.serialize publicKey)
    let ripemd160 = new RIPEMD160Managed()
    return ripemd160.ComputeHash(hash)
}

let Hardened = (+) 0x80000000
let private isHardened i = i < 0

let private ser32 (i : int32) =
    let bytes = BitConverter.GetBytes i
    if System.BitConverter.IsLittleEndian
    then Array.rev bytes
    else bytes

let private ser256 = toBytes32
let private parse256 = fromBytes32

let private computeHash chainCode data =
    let hmacsha512 = new HMACSHA512(chainCode)
    let i = hmacsha512.ComputeHash(data:byte[])
    i.[0..31], i.[32..63]

let private (++) a b = Array.append a b

let create seed =
    let il, ir = computeHash (Encoding.ASCII.GetBytes preSeed) seed

    if il = Array.zeroCreate 32 || il >= secp256k1.n then
        Error "invalid seed"
    else
        Ok {
            key = ExtendedPrivateKey (SecretKey il)
            depth = 0uy
            chainCode = ir
            parentFingetprint = Array.zeroCreate 4
            index = 0
        }

let fromMnemonicPhrase (mnemonicPhrase:string) =
    try
         let mnemonicSentence = new NBitcoin.Mnemonic(mnemonicPhrase, NBitcoin.Wordlist.English)

         mnemonicSentence.DeriveSeed ()
         |> create
    with _ as ex ->
        Error ex.Message

let derive i parent = result {
    let! data = result {
        match parent.key with
        | ExtendedPrivateKey privateKey ->
            if isHardened i then
                return [| 0uy |] ++ (SecretKey.serialize privateKey)
            else
                let! publicKey = getPublicKey parent
                return PublicKey.serialize publicKey
        | ExtendedPublicKey publicKey ->
            if isHardened i then
                yield! Error "must not be hardened"
            return PublicKey.serialize publicKey
    }

    let il, ir = computeHash parent.chainCode (data ++ ser32 i)
    if il >= secp256k1.n then yield! Error "cannot derive"

    let! key = result {
        match parent.key with
        | ExtendedPrivateKey (SecretKey prvBytes) ->
            let key = ((parse256 il) + parse256 prvBytes) % (parse256 secp256k1.n)
            if key = 0I then yield! Error "cannot derive"
            return
                key
                |> ser256
                |> SecretKey
                |> ExtendedPrivateKey
        | ExtendedPublicKey _ ->
            yield! Error "not supported"
    }

    let! identifier = getIdentifier parent
    return {
        key = key
        chainCode = ir
        depth = parent.depth + 1uy
        parentFingetprint = identifier.[0..3]
        index = i
    }
}

let neuter parent = result {
    let! publicKey = getPublicKey parent
    return { parent with key = ExtendedPublicKey publicKey }
}

let encode chainKeyArgs extendedKey = result {
    let data, chain =
        match extendedKey.key with
        | ExtendedPublicKey publicKey ->
            PublicKey.serialize publicKey, chainKeyArgs.Public
        | ExtendedPrivateKey privateKey->
            [| 0uy |] ++ (SecretKey.serialize privateKey), chainKeyArgs.Private
    return
        chain
        ++ [| extendedKey.depth |]
        ++ extendedKey.parentFingetprint
        ++
        ser32 extendedKey.index
        ++ extendedKey.chainCode
        ++ data
        |> Base58Check.Base58CheckEncoding.Encode
}

let derivePath path extendedKey = result {
    let! indexes = KeyPathParser.parse path

    return! List.fold (fun extendedKey index -> extendedKey >>= derive index) (Ok extendedKey) indexes
}

let sign msg extendedKey = result {
    let! secretKey = getPrivateKey extendedKey

    return Crypto.sign secretKey msg
}