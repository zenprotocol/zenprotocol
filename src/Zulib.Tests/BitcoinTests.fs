module Zulib.Tests.BitcoinTests

open NUnit.Framework
open System.Text
open FsUnit
open Infrastructure
open FsCheck.NUnit
open FStar.Pervasives.Native
open FsBech32

open Helper

[<Literal>]
let headerBase16 = "02000000b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c00000000000000009d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab3147124d95a5430c31b18fe9f0864"

let headerBase16Bytes = Encoding.ASCII.GetBytes headerBase16

[<Test>]
let ``Real bitcoin header should parse``() =
    let res =   match unCost <| Zen.Bitcoin.parseHeader headerBase16Bytes with
                | Some _ -> true
                | None -> false
    res |> should equal true

[<Test>]
let ``Real bitcoin header should have correct parent``() =
    let headerOpt = unCost <| Zen.Bitcoin.parseHeader headerBase16Bytes
    let firstBytes =
        match headerOpt with
        | Some bs -> Zen.Bitcoin.parent bs |> unCost |> Array.take 5
        | None -> [||]
    firstBytes |> should equal [| 0xb6uy; 0xffuy; 0x0buy; 0x1buy; 0x16uy |]

[<Test>]
let ``Real bitcoin header should have correct nbits``() =
    let headerOpt = unCost <| Zen.Bitcoin.parseHeader headerBase16Bytes
    let nbits =
        match headerOpt with
        | Some bs -> unCost <| Zen.Bitcoin.nbits bs
        | None -> [||]
    nbits |> should equal [| 0x30uy; 0xc3uy; 0x1buy; 0x18uy |]

[<Test>]
let ``Real bitcoin header should have correct hash``() =
    let headerOpt = unCost <| Zen.Bitcoin.parseHeader headerBase16Bytes
    let headerHash =
        match headerOpt with
        | Some bs -> unCost <| Zen.Bitcoin.computeHeaderHash bs
        | None -> [||]
    let realHash =
        "000000000000000009a11b3972c8e532fe964de937c9e0096b43814e67af3728"
        |> Base16.decode |> Option.get |> Array.rev
    headerHash
    |> should equal realHash

[<Test>]
let ``Real bitcoin header should pass difficulty target``() =
    let headerOpt = unCost <| Zen.Bitcoin.parseHeader headerBase16Bytes
    let headerHash =
        match headerOpt with
        | Some bs -> unCost <| Zen.Bitcoin.computeHeaderHash bs
        | None -> Array.create 32 0xffuy
    let nbits =
        match headerOpt with
        | Some bs -> unCost <| Zen.Bitcoin.nbits bs
        | None -> [||]
    (unCost <| Zen.Bitcoin.checkProofOfWork headerHash nbits)
    |> should equal true

[<Test>]
let ``High hash should not be LTE target``() =
    let highHash = Array.create 32 0xffuy
    let nbits = [| 0x30uy; 0xc3uy; 0x1buy; 0x18uy |]
    (unCost <| Zen.Bitcoin.checkProofOfWork highHash nbits)
    |> should equal false

[<Test>]
let ``Real bitcoin headers give correct next target``() =
    // Block #524159
    let lastHeaderBytes =
        "0000002084a4ee3e216e9f5fe75067dd9679711268c178caffa441000000000000000000d04085631585f83736dbbc7ef7f9f27ea6b0765d2c79aa2cd25c2d1c659014a2e1b9065ba9ec431726f37cc3"
        |> Base16.decode |> Option.get
    // Block #522144
    let firstHeaderBytes =
        "00000020d66bd48556c8a7cede3821a0344817132cab9b364dec18000000000000000000ab5ccc0455785c744c08990740c6ef8006013553638a1856cc030d14efe94c68c7f7f45aa9ec4317410c2249"
        |> Base16.decode |> Option.get
    let nextNBits =
        unCost
        <| Zen.Bitcoin.calculateNextWorkRequired firstHeaderBytes lastHeaderBytes
    // Block # 533160 has bits = 390158921 == 0x17415a49
    nextNBits |> should equal [| 0x49uy; 0x5auy; 0x41uy; 0x17uy |]