module Infrastructure.ZFStar.Tests

open NUnit.Framework
open FsUnit
open System.Numerics

let fstCode = """
val main: nat -> nat
let main i = i + 1
"""

let (>=>) a b = Result.bind b a

[<Test>]
let ``Should invoke compiled``() =
    let input = 10I
    let output = 11I

    let result = 
        ZFStar.compile "TestModule" fstCode
        >=>
        (fun assembly ->
            try 
                Ok (assembly
                .GetModules().[0]
                .GetTypes().[0]
                .GetMethods().[0])
            with | _ -> Error "could not access method")
        >=>
        (fun methodInfo ->
            try 
                Ok (methodInfo.Invoke(null, [| input |]))
            with
                | _ -> Error "unable to invoke method")
        >=>
        (fun result ->
            try 
                Ok (result :?> BigInteger)
            with
                | _ -> Error "unexpected result")
 
    result |> should equal (Ok output : Result<BigInteger, string>)