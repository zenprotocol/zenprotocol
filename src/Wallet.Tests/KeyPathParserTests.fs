module KeyPathParserTests

open NUnit.Framework
open FsUnit
open Wallet
open ExtendedKey

[<Test>]
let ``can parse valid key path``() =
    let path = KeyPathParser.parse "m/44'/258'/0'/0/0"

    printfn "%A" path

    let expected:Result<int list,string> = Ok [Hardened 44;Hardened 258;Hardened 0;0;0]

    printfn "%A" expected

    path |> should equal <| expected