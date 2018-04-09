module Zen.Crypto.fs

open System.Runtime.InteropServices
open Zen.Types.Extracted

module Cost = Zen.Cost.Realized

module Native =
    let SECP256K1_FLAGS_TYPE_CONTEXT = (1ul <<< 0)
    let SECP256K1_FLAGS_BIT_CONTEXT_VERIFY = (1ul <<< 8)
    let SECP256K1_CONTEXT_VERIFY = (SECP256K1_FLAGS_TYPE_CONTEXT ||| SECP256K1_FLAGS_BIT_CONTEXT_VERIFY)

    type Context = System.IntPtr

    type Result =
        | Error = 0
        | Ok = 1

    // Create a secp256k1 context object.
    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Context secp256k1_context_create(uint32 flags);

    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Result secp256k1_ecdsa_verify(
        Context ctx,
        byte[] signature,
        byte[] msg32,
        byte[] pubkey
    );

let private context = Native.secp256k1_context_create (Native.SECP256K1_CONTEXT_VERIFY)

let verify (pk:publicKey) (signature:signature) (msg:hash) : Cost.t<bool, unit> =
    lazy (
        match Native.secp256k1_ecdsa_verify (context, signature, msg, pk) with
        | Native.Result.Ok -> true
        | _ -> false
    )
    |> Cost.C
