module Infrastructure.Platform.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO

let testExeDir = 
    try 
        List.find Directory.Exists
            //TODO: Linux, Windows
            ["/Library/Frameworks/Mono.framework/Versions/Current/lib/mono/fsharp/"]
    with 
    | _ ->
        "Cannot find directory"
        |> Exception
        |> raise

let fsc_exe = System.IO.Path.Combine (testExeDir, "fsc.exe")

[<Test>]
let ``Should get output from process``() =
    (Platform.run fsc_exe [ "--help" ]) |> should equal (Ok (): Result<unit, string>)

[<Test>]
let ``Should get error from process``() =
    (Platform.run fsc_exe) |> should equal (Error "error FS0207: No inputs specified": Result<unit, string>)