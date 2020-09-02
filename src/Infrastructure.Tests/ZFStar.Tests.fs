module Infrastructure.Tests.ZFStar

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

let assemblyDirectory =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]

[<Literal>]
let rlimit = 2723280u

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

let compile fstCode =
    let moduleName = getModuleName fstCode
    ZFStar.recordHints fstCode moduleName
    |> Result.bind (fun hints -> ZFStar.compile assemblyDirectory fstCode hints rlimit moduleName)
    |> Result.bind (fun _ -> ZFStar.load assemblyDirectory moduleName)

let fstCode = """
    open Zen.Types
    open Zen.Base
    open Zen.Cost
    open Zen.ResultT

    module C = Zen.Cost
    
    let main tx _ _ _ _ _ _ _ =
        ok @ { tx = tx; message = None; state = NoChange }

    let cf _ _ _ _ _ _ _ = 
        0 + 5
        |> cast nat
        |> C.ret
    """

[<Test>]
let ``Should record hints``() =
    ZFStar.recordHints fstCode (getModuleName fstCode)
    |> Result.map (fun _ -> ())
    |> shouldBeOk ()

[<Test>]
let ``Should compile``() =
    compile fstCode
    |> Result.map (fun _ -> ())
    |> shouldBeOk ()

[<Test>]
let ``Should get some metrics from hints module``() =
    ZFStar.recordHints fstCode (getModuleName fstCode)
    |> Result.bind (ZFStar.calculateMetrics)
    |> Result.map (fun (maxFuel, maxIFuel) -> maxFuel > 0 && maxIFuel > 0)
    |> shouldBeOk true
