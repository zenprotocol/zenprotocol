module Consensus.Crypto

open System.Runtime.InteropServices
open Consensus.Hash

module secp256k1 =
    let n =
        [|
            0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy;
            0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy;
            0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy;
            0xFFuy; 0xFFuy; 0xFFuy; 0xFEuy;
            0xBAuy; 0xAEuy; 0xDCuy; 0xE6uy;
            0xAFuy; 0x48uy; 0xA0uy; 0x3Buy;
            0xBFuy; 0xD2uy; 0x5Euy; 0x8Cuy;
            0xD0uy; 0x36uy; 0x41uy; 0x41uy;
        |]

module Native =
    type Context = System.IntPtr

    type Result =
        | Error = 0
        | Ok = 1

    //All flags' lower 8 bits indicate what they're for. Do not use directly.
    let SECP256K1_FLAGS_TYPE_MASK = ((1ul <<< 8) - 1ul)
    let SECP256K1_FLAGS_TYPE_CONTEXT = (1ul <<< 0)
    let SECP256K1_FLAGS_TYPE_COMPRESSION = (1ul <<< 1)

    // The higher bits contain the actual data. Do not use directly.
    let SECP256K1_FLAGS_BIT_CONTEXT_VERIFY = (1ul <<< 8)
    let SECP256K1_FLAGS_BIT_CONTEXT_SIGN = (1ul <<< 9)
    let SECP256K1_FLAGS_BIT_COMPRESSION = (1ul <<< 8)

    // Flags to pass to secp256k1_context_create.
    let SECP256K1_CONTEXT_VERIFY = (SECP256K1_FLAGS_TYPE_CONTEXT ||| SECP256K1_FLAGS_BIT_CONTEXT_VERIFY)
    let SECP256K1_CONTEXT_SIGN = (SECP256K1_FLAGS_TYPE_CONTEXT ||| SECP256K1_FLAGS_BIT_CONTEXT_SIGN)
    let SECP256K1_CONTEXT_NONE = (SECP256K1_FLAGS_TYPE_CONTEXT)

    // Flag to pass to secp256k1_ec_pubkey_serialize and secp256k1_ec_privkey_export.
    let SECP256K1_EC_COMPRESSED = (SECP256K1_FLAGS_TYPE_COMPRESSION ||| SECP256K1_FLAGS_BIT_COMPRESSION)
    let SECP256K1_EC_UNCOMPRESSED = (SECP256K1_FLAGS_TYPE_COMPRESSION)

    // Prefix byte used to tag various encoded curvepoints for specific purposes
    let SECP256K1_TAG_PUBKEY_EVEN = 0x02ul
    let SECP256K1_TAG_PUBKEY_ODD = 0x03ul
    let SECP256K1_TAG_PUBKEY_UNCOMPRESSED = 0x04ul
    let SECP256K1_TAG_PUBKEY_HYBRID_EVEN = 0x06ul
    let SECP256K1_TAG_PUBKEY_HYBRID_ODD = 0x07ul

    // Create a secp256k1 context object.
    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Context secp256k1_context_create(uint32 flags);

    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern void secp256k1_context_destroy(Context ctx);

    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Result secp256k1_ec_pubkey_parse(
        Context ctx,
        byte[] pubkey,
        byte[] input,
        uint32 inputlen
    );

    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Result secp256k1_ec_pubkey_serialize(
        Context ctx,
        byte[] output,
        int64 *outputlen,
        byte[] pubkey,
        uint32 flags
    );

    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Result secp256k1_ecdsa_signature_parse_compact(
        Context ctx,
        byte[] signature,
        byte[] input64
    );

    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Result secp256k1_ecdsa_signature_serialize_compact(
        Context ctx,
        byte[] output64,
        byte[] signature
    );

    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Result secp256k1_ecdsa_verify(
        Context ctx,
        byte[] signature,
        byte[] msg32,
        byte[] pubkey
    );

    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Result secp256k1_ecdsa_sign(
        Context ctx,
        byte[] signature,
        byte[] msg32,
        byte[] seckey,
        System.IntPtr noncefp,
        System.IntPtr ndata
    );

    //TODO: see ExtendedKey in Wallet
//    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
//    extern Result secp256k1_ec_pubkey_tweak_add(
//        Context ctx,
//        byte[] pubkey,
//        byte[] tweak
//    );

    [<DllImport("secp256k1")>]
    extern Result secp256k1_ec_pubkey_create(
        Context ctx,
        byte[] pubkey,
        byte[] seckey
    );

[<Literal>]
let private SecretKeyLength = 32

[<Literal>]
let private PublicKeyLength = 64

[<Literal>]
let private SignatureLength = 64

[<Literal>]
let SerializedPublicKeyLength = 33

[<Literal>]
let SerializedSignatureLength = 64

[<Literal>]
let MessageLength = 32

type VerifyResult =
    | Valid
    | Invalid

type SecretKey = SecretKey of array<byte>
type PublicKey = PublicKey of array<byte>
type Signature = Signature of array<byte>
type KeyPair = SecretKey * PublicKey

let private rng = new System.Security.Cryptography.RNGCryptoServiceProvider()
let private context = Native.secp256k1_context_create (Native.SECP256K1_CONTEXT_SIGN ||| Native.SECP256K1_CONTEXT_VERIFY)

module SecretKey =
    let getPublicKey (SecretKey secretKey) =

        let publicKey = Array.create PublicKeyLength 0uy

        match Native.secp256k1_ec_pubkey_create (context, publicKey, secretKey) with
        | Native.Result.Ok -> Some (PublicKey.PublicKey publicKey)
        | Native.Result.Error ->  None
        | x -> failwithf "Unexpected result %A" x

    let serialize (SecretKey secretKey) =
        secretKey

#nowarn "51"
module PublicKey =
    let serialize (PublicKey publicKey) =
        let bytes = Array.create SerializedPublicKeyLength 0uy
        let mutable length = int64 SerializedPublicKeyLength

        match Native.secp256k1_ec_pubkey_serialize(context, bytes, &&length, publicKey, Native.SECP256K1_EC_COMPRESSED) with
        | Native.Result.Ok ->
            if 33L = length then bytes else failwith "Wrong serialized size"
        | _ -> failwith "failed to serialize public key"

    let deserialize bytes =
        if Array.length bytes <> 33 then None
        else
            let publicKey = Array.create PublicKeyLength 0uy
            match Native.secp256k1_ec_pubkey_parse(context, publicKey, bytes, 33ul) with
            | Native.Result.Ok -> Some (PublicKey publicKey)
            | _ -> None

    let toString = serialize >> FsBech32.Base16.encode

    let hash = serialize >> Hash.compute

module Signature =
    let serialize (Signature signature) =
        let bytes = Array.create SerializedSignatureLength 0uy

        match Native.secp256k1_ecdsa_signature_serialize_compact (context, bytes, signature) with
        | Native.Result.Ok -> bytes
        | _-> failwith "failed to serialize signature"

    let deserialize bytes =
        let signature = Array.create SignatureLength 0uy
        match Native.secp256k1_ecdsa_signature_parse_compact (context, signature, bytes) with
        | Native.Result.Ok -> Some (Signature signature)
        | _ -> None

    let toString = serialize >> FsBech32.Base16.encode

    let fromString b16 = FsBech32.Base16.decode b16 |> Option.bind deserialize

module KeyPair =
    let fromSecretKey secretKey =
        match  SecretKey.getPublicKey secretKey with
        | Some publicKey -> secretKey,publicKey
        | None -> failwith "invalid publickey"

    let rec create () : KeyPair =
        let secretKey = Array.create SecretKeyLength 0uy
        rng.GetBytes (secretKey)

        let publicKey = Array.create PublicKeyLength 0uy

        match Native.secp256k1_ec_pubkey_create (context, publicKey, secretKey) with
        | Native.Result.Ok -> SecretKey secretKey, PublicKey.PublicKey publicKey
        | Native.Result.Error -> create ()
        | x -> failwithf "Unexpected result %A" x

let sign (SecretKey secretKey) (Hash msg) =
    let signature = Array.create SignatureLength 0uy

    match Native.secp256k1_ecdsa_sign (context, signature, msg, secretKey, System.IntPtr.Zero, System.IntPtr.Zero) with
    | Native.Result.Ok -> Signature.Signature signature
    | x -> failwithf "failed to sign %A" x

let verify (PublicKey publicKey) (Signature signature) (Hash msg) =
    match Native.secp256k1_ecdsa_verify (context, signature, msg, publicKey) with
    | Native.Result.Ok -> Valid
    | _ -> Invalid