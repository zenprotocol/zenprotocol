module Infrastructure.ZFStar.Tests

open NUnit.Framework
open Infrastructure
open TestsInfrastructure.Nunit
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

let assemblyDirectory = "./test"

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

let fstCode = """
    open Zen.Types
    open Zen.Vector
    open Zen.Util
    open Zen.Base
    open Zen.Cost
    open Zen.ErrorT

    val cf: txSkeleton -> string -> #l:nat -> wallet l -> cost nat 1
    let cf _ _ #l _ = ~!2

    val main: txSkeleton -> hash -> string -> #l:nat -> wallet l -> cost (result txSkeleton) 2
    let main tx chash command #l _ =
        ret @ tx
    """

[<Test>]
let ``Should record hints``() =
    (ZFStar.recordHints fstCode (getModuleName fstCode)
    |> Result.map (fun _ -> ())
    |> Result.map (fun _ -> ())
    , (Ok() : Result<unit, string>))
    |> shouldEqual

[<Test>]
let ``Should invoke compiled``() =
    (compileAndInvoke fstCode
    [| input; null; null; 0I; null |]
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

        val cf: txSkeleton -> string -> #l:nat -> wallet l -> cost nat 1
        let cf _ _#l _ = ~!1

        val main: txSkeleton -> hash -> string -> #l:nat -> wallet l -> cost (result txSkeleton) 1
        let main tx chash command #l _ =
            failw command
        """
    [| null; null; "test command";0I;null |]
    , (Error "test command" : Result<txSkeleton,string>))
    |> shouldEqual

[<Test>]
let ``Should get some metrics from hints module``() =
    (ZFStar.recordHints fstCode (getModuleName fstCode) 
    |> Result.bind (ZFStar.calculateMetrics) 
    |> Result.map (fun (maxFuel, maxIFuel) -> maxFuel > 0 && maxIFuel > 0)
    , (Ok true : Result<bool, string>)) |> shouldEqual