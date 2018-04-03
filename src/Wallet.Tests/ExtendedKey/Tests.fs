module Wallet.Tests.ExtendedKey.Tests

open NUnit.Framework

open Infrastructure.Result
open Wallet.ExtendedKey

open Helpers
open Constants
open Expected

open Vector1

[<Test>]
let ``TestVector1``() = 
    create seed m
    >>= (derive (Hardened 0) m_0H "m/0H"
        >=> derive 1 m_0H_1 "m/0H/1"
        >=> derive (Hardened 2) m_0H_1_2H "m/0H/1/2H"
        >=> derive 2 m_0H_1_2H_2 "m/0H/1/2H/2"
        >=> derive 1000000000 m_0H_1_2H_2_1000000000 "m/0H/1/2H/2/1000000000")
    |> shouldOk

open Vector2

[<Test>]
let ``TestVector2``() = 
    create seed m
    >>= (derive 0 m_0 "m/0"
        >=> derive (Hardened 2147483647) m_0_2147483647H "m/0/2147483647H"
        >=> derive 1 m_0_2147483647H_1 "m/0/2147483647H/1"
        >=> derive (Hardened 2147483646) m_0_2147483647H_1_2147483646H "m/0/2147483647H/1/2147483646H"
        >=> derive 2 m_0_2147483647H_1_2147483646H_2 "m/0/2147483647H/1/2147483646H/2")
    |> shouldOk

//[<Test>]
//let ``Public derivation (using TestVector2)``() = 
//    create preSeed seed m
//    >>= neuter
//    >>= derive 0 m_0 "m/0"
//    |> shouldOk

open Vector3

[<Test>]
let ``TestVector3``() = 
    create seed m
    >>= (derive (Hardened 0) m_0H "m/0H")
    |> shouldOk

[<Test>]
let ``Should not public-derive for a hardened index``() =
    Wallet.ExtendedKey.create seed
    >>= neuter
    >>= Wallet.ExtendedKey.derive (Hardened 1)
    |> shouldError "must not be hardened"