module Zen.Merkle

open FStar.Pervasives
open System
open Zen.Types.Extracted
open Zen.Option
module ZArr = Zen.Array.Extracted
module Cost = Zen.Cost.Realized
module sha3 = Zen.Sha3.Realized

let serialize : data<Prims.unit> -> Native.option<byte []> = function
    // Oracle data structure
    | Data4 (_, _, _, _,
      ByteArray (_, underlyingBytes),
      UInt64 price,
      UInt64 timestamp,
      Hash nonce) ->
        [ underlyingBytes; BitConverter.GetBytes price; BitConverter.GetBytes timestamp ]
        |> List.fold (fun acc elem -> Array.append elem acc) [||]
        |> Native.Some
    | _ -> Native.None

let hashData
    ( _ : Prims.nat)
    data
    : Cost.t<hash Native.option, Prims.unit> =
        lazy (
            map sha3.hash256 <| serialize data
        )
        |> Cost.C

let rootFromAuditPath
    ( _: Prims.nat)
    ( item : hash )
    ( location: uint32 )
    ( hashes : ZArr.t<hash, Prims.unit> )
    : Cost.t<hash, Prims.unit> =
        lazy (
            fst <|
            Array.fold
                (fun (v, loc) h ->
                    if loc % 2u = 0u
                    then
                        (sha3.hash256 <| Array.append v h, loc >>> 1)
                    else
                        (sha3.hash256 <| Array.append h v, loc >>> 1))
                (item,uint32 location) hashes
        )
        |> Cost.C
