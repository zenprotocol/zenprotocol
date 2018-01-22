module Infrastructure.ZFStar.Tests

open NUnit.Framework
open FsUnit 
open Infrastructure

let fstCode = """
val test: nat -> nat
let test i = i + 1
"""
let input = 10I
let output = 11I

let (>=>) a b = Result.bind b a

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
                .GetMethods().[0])
            with | _ -> Error "could not access method")
        |> Result.bind (fun methodInfo ->
            try 
                Ok (methodInfo.Invoke(null, [| input |]))
            with
                | _ -> Error "unable to invoke method")
        |> Result.bind (fun result ->
            try 
                Ok (result :?> System.Numerics.BigInteger)
            with
                | _ -> Error "unexpected result")

    should equal result (Ok output : Result<System.Numerics.BigInteger,string>)