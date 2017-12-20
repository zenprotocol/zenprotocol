module Consensus.Tests.ContractTests

open Consensus
open NUnit.Framework
open FsUnit

let fst = "val test: nat -> nat\nlet test i = i + 1"

[<Test>]
let ``Should get contract function``() = 
    let i = 1I
    let fn = fun i -> i + 1I
    match Contract.compile fst
         with
    | Ok (_, cfn) ->
        match Contract.run cfn i with 
        | Ok res -> res |> should equal (fn i)
        | Error err -> failwith err
    | Error err ->
        failwith err

[<Test>]
let ``Should get valid hash``() = 
    match Contract.compile fst with
    | Ok (hash, _) -> 
        Hash.isValid hash |> should equal true 
    | Error err ->
        failwith err

