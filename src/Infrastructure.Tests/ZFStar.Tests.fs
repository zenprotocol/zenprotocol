﻿module Infrastructure.ZFStar.Tests

open NUnit.Framework
open Infrastructure
open TestsInfrastructure.Nunit
open Exception
open Zen.Types.Extracted
open Zen.Types.TxSkeleton
open FStar.Pervasives
open Microsoft.FSharp.Core
open Zen.Cost.Realized
open System.Text
open Org.BouncyCastle.Crypto.Digests
open FsBech32

let assemblyDirectory = "./data"

let clean =
    Platform.cleanDirectory assemblyDirectory

[<SetUp>]
    clean

[<TearDown>]
    clean

let input : txSkeleton =
    emptyTxSkeleton


let computeHash bytes =
    let hash = Array.zeroCreate 32
    let sha3 = new Sha3Digest(256)
    sha3.BlockUpdate(bytes,0,Array.length bytes)
    sha3.DoFinal(hash, 0) |> ignore
    hash

let getModuleName (code : string) =
    code
    |> Encoding.UTF8.GetBytes
    |> computeHash
    |> Base16.encode

let compileAndInvoke fstCode args =
    ZFStar.compile assemblyDirectory fstCode (getModuleName fstCode)
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
            Ok (methodInfo.Invoke(null, args))
        with _ as ex ->
            Exception.toError "unable to invoke method" ex)
    |> Result.bind (fun result ->
        try
            Ok (result :?> cost<result<txSkeleton>, unit>)
        with _ as ex ->
            Exception.toError "unexpected result" ex)
    |> Result.map (
        fun (Zen.Cost.Realized.C inj:cost<result<txSkeleton>, unit>) ->
            inj.Force()
    )
    |> Result.bind (function
        | OK value -> Ok value
        | ERR err -> Error err
        | EX err -> Error err.Message //TODO: remove EX
    )

[<Test>]
let ``Should invoke compiled``() =
    (compileAndInvoke """
        open Zen.Types
        open Zen.Vector
        open Zen.Util
        open Zen.Base
        open Zen.Cost
        open Zen.ErrorT

        val cf: txSkeleton -> string -> cost nat 1
        let cf _ _ = ~!2

        val main: txSkeleton -> hash -> string -> cost (result txSkeleton) 2
        let main tx chash command =
            ret @ tx
        """
    [| input; null; null |]
    , (Ok input : Result<txSkeleton,string>))
    |> shouldEqual

[<Test>]
let ``Should throw with command's value``() =
    (compileAndInvoke """
        open Zen.Types
        open Zen.Vector
        open Zen.Util
        open Zen.Base
        open Zen.Cost
        open Zen.ErrorT

        val cf: txSkeleton -> string -> cost nat 1
        let cf _ _ = ~!1

        val main: txSkeleton -> hash -> string -> cost (result txSkeleton) 1
        let main tx chash command =
            failw command
        """
    [| null; null; "test command" |]
    , (Error "test command" : Result<txSkeleton,string>))
    |> shouldEqual
