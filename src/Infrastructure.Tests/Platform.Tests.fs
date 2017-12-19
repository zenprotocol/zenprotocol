module Infrastructure.Platform.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO

let currentDirectory = (new FileInfo(System.Reflection.Assembly.GetExecutingAssembly().Location)).DirectoryName
let fsc_exe = 
    Path.Combine(currentDirectory,  "../../../../packages/FSharp.Compiler.Tools/tools/fsc.exe")

[<Test>]
let ``Should get output from process``() =
    printfn "%s" fsc_exe
    (Platform.run fsc_exe [ "--help" ]) |> should equal (Ok (): Result<unit, string>)

[<Test>]
let ``Should get error from process``() =
    let result = Platform.run fsc_exe [] 

    printfn "%A" result

    result |> should equal (Error "run: error FS0207: No inputs specified": Result<unit, string>)