module Infrastructure.ZFStar.Tests

open NUnit.Framework
open Infrastructure
open FsUnit
open Exception
open Zen.Types.Extracted
open Zen.Types.Realized
open Zen.TxSkeleton
open FStar.Pervasives
open Microsoft.FSharp.Core
open Zen.Cost.Realized
open System.Text
open Org.BouncyCastle.Crypto.Digests
open FsBech32
open Zen.Types.Main

let assemblyDirectory = "./test"

type Message = {
    cHash: byte[]
    command: string
}

let clean() =
    Platform.cleanDirectory assemblyDirectory

[<SetUp>]
let setup = fun () ->
    clean()

[<TearDown>]
let tearDown = fun () ->
    clean()

let shouldBeOk value = function
    | Ok value' -> should equal value value'
    | Error err -> failwith err

let shouldBeError err = function
    | Ok value' -> failwith (value'.ToString())
    | Error err' -> should equal err err'

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
    |> (+) "Test"

let compileAndInvoke fstCode args =
    let moduleName = getModuleName fstCode
    ZFStar.recordHints fstCode moduleName
    |> Result.map (fun hints -> (fstCode, hints))
    |> Result.bind (fun (code, hints) -> ZFStar.compile assemblyDirectory (code, hints) moduleName)
    |> Result.bind (fun _ -> ZFStar.load assemblyDirectory moduleName)
    |> Result.bind (fun assembly ->
        try
            Ok (assembly
            .GetModules().[0]
            .GetTypes().[0]
            .GetMethod("main"))
        with _ as ex ->
            Exception.toError "could not find method" ex)
    |> Result.bind (fun methodInfo ->
        try
            Ok (methodInfo.Invoke(null, args))
        with _ as ex ->
            Exception.toError "unable to invoke method" ex)
    |> Result.bind (fun result ->
        try
            Ok (result :?> cost<result<txSkeleton * message Native.option>, unit>)
        with _ as ex ->
            Exception.toError "unexpected result" ex)
    |> Result.map (
        fun (Zen.Cost.Realized.C inj:cost<result<txSkeleton * message Native.option>, unit>) ->
            inj.Force()
    )
    |> Result.bind (function
        | OK value -> Ok value
        | ERR err -> Error (System.Text.Encoding.ASCII.GetString err)
        | EX err -> Error err.Message //TODO: remove EX
    )

let fstCode = """
    open Zen.Types
    open Zen.Base
    open Zen.Cost
    open Zen.ResultT

    val main: txSkeleton -> hash -> string -> option data -> wallet
        -> result (txSkeleton ** option message) `cost` 4
    let main tx chash command data _ =
        ok @ (tx, None)

    val cf: txSkeleton -> string -> option data -> wallet -> cost nat 1
        let cf _ _ _ _ = ~!4
    """

[<Test>]
[<ParallelizableAttribute>]
let ``Should record hints``() =
    ZFStar.recordHints fstCode (getModuleName fstCode)
    |> Result.map (fun _ -> ())
    |> shouldBeOk ()

[<Test>]
[<ParallelizableAttribute>]
let ``Should invoke compiled``() =
    compileAndInvoke fstCode [| input; null; null; null; null |]
    |> shouldBeOk (input, Native.option<message>.None)

[<Test>]
[<ParallelizableAttribute>]
let ``Should throw with command's value``() =
    compileAndInvoke """
        open Zen.Types
        open Zen.Base
        open Zen.Cost
        open Zen.ResultT

        val main: txSkeleton -> hash -> string -> option data -> wallet
            -> result (txSkeleton ** option message) `cost` 1
        let main tx chash command data _ =
            failw command

        val cf: txSkeleton -> string -> option data -> wallet -> cost nat 1
                let cf _ _ _ _ = ~!1
        """ [| null; null; "test command"B; null; null |]
    |> shouldBeError "test command"

[<Test>]
[<ParallelizableAttribute>]
let ``Should get some metrics from hints module``() =
    ZFStar.recordHints fstCode (getModuleName fstCode)
    |> Result.bind (ZFStar.calculateMetrics)
    |> Result.map (fun (maxFuel, maxIFuel) -> maxFuel > 0 && maxIFuel > 0)
    |> shouldBeOk true
