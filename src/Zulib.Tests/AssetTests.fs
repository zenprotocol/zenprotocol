module Zulib.Tests.AssetTests

open NUnit.Framework
open System.Text
open FsUnit
open Infrastructure
open Consensus
open FsBech32
   
module Cost = Zen.Cost.Realized

let unCost (Cost.C inj:Zen.Cost.Realized.cost<'Aa, 'An>) : 'Aa = inj.Force()

[<Test>]
let ``Should create asset from string``() =
    let bytes = 
        Array.create 10 (byte 'A') 
        |> Encoding.UTF8.GetString
        
    let asset = Zen.Asset.fromString Zen.Asset.zeroHash bytes
    
    let contractHash, tokenHash = unCost asset

    let concated = Array.concat [ contractHash; tokenHash ]

    let encoded = Base16.encode concated

    printf "encoded value: %A" encoded

[<Test>]
let ``Should create asset from bigint``() =
    Zen.Asset.fromInt Zen.Asset.zeroHash 63456u
