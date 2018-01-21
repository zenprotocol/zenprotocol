module Infrastructure.ZFStar.Tests

open NUnit.Framework
open Infrastructure
open TestsInfrastructure.Nunit
open Exception
open Zen.Types.Extracted
open FStar.Pervasives
open Microsoft.FSharp.Core
open Zen.Cost.Realized

let fstCode = """
open Zen.Types
open Zen.Vector
open Zen.Util
open Zen.Base
open Zen.Cost
open Zen.ErrorT

val cf: transactionSkeleton -> cost nat 1
let cf _ = ~!2

val main: transactionSkeleton -> hash -> cost (result transactionSkeleton) 2
let main transactionSkeleton hash =
    ret @ transactionSkeleton
"""

let input =
    Tx (
        0I,
        Zen.Vector.VNil,
        0I,
        Zen.Vector.VNil,
        Native.option.None
    )

[<Test>]
let ``Should invoke compiled``() =
    let assemblyDirectory = "./data"

    Platform.cleanDirectory assemblyDirectory

    let result =
        ZFStar.compile assemblyDirectory fstCode "Test"
        |> Result.bind (fun assembly ->
            try
                Ok (assembly
                .GetModules().[0]
                .GetTypes().[0]
                .GetMethods().[1])
            with _ as ex ->
                Exception.toError "could not find method" ex)
        |> Result.bind (fun methodInfo ->
            try
                Ok (methodInfo.Invoke(null, [| input; null |]))
            with _ as ex ->
                Exception.toError "unable to invoke method" ex)
        |> Result.bind (fun result ->
            try
                Ok (result :?> cost<result<transactionSkeleton>, unit>)
            with _ as ex ->
                Exception.toError "unexpected result" ex)
        |> Result.map (
            fun (Zen.Cost.Realized.C inj:cost<result<transactionSkeleton>, unit>) ->
                inj.Force()
        )
        |> Result.bind (function
            | OK value -> Ok value
            | ERR err -> Error err
            | EX err -> Error err.Message //TODO: remove EX
        )

    shouldEqual (result, (Ok input : Result<transactionSkeleton,string>))
