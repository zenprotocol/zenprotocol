#r @"packages/FAKE/tools/FakeLib.dll"
#r @"packages/Zen.FSharp.Compiler.Service/lib/net45/Zen.FSharp.Compiler.Service.dll"

open Fake
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler
  
Target "Zulib.Extract" (fun _ ->  

  let files =
    FileSystemHelper.directoryInfo  FileSystemHelper.currentDirectory
    |> FileSystemHelper.filesInDirMatching "Zulib/fstar/*.fst*"
    |> Array.map (fun file -> file.FullName)

  let args = 
    [| "--lax";"--codegen";"FSharp";
       "--prims";"Zulib/fstar/prims.fst";
       "--extract_module";"Zen.Base";
       "--extract_module";"Zen.Option";
       "--extract_module";"Zen.Cost.Extracted";
       "--codegen-lib";"Zen.Cost";
       //"--extract_module";"Zen.Cost";
       "--extract_module";"Zen.OptionT";
       "--extract_module";"Zen.Tuple";
       "--extract_module";"Zen.TupleT";
       "--extract_module";"Zen.Vector";
       "--extract_module";"Zen.Array.Extracted";
       "--codegen-lib";"Zen.Array";       
       //"--extract_module";"Zen.Array";
       "--extract_module";"Zen.Types.Extracted";
       "--codegen-lib";"Zen.Types";     
       //"--extract_module";"Zen.Types";
       "--odir";"Zulib/fsharp/Extracted"; |] 
       |> Array.append files
       |> Array.reduce (fun a b -> a + " " + b)  

  let exitCode = 
    ProcessHelper.Shell.Exec ("../../FStar/bin/fstar.exe", args)

  if exitCode <> 0 then    
    failwith "extracting Zulib failed"
)

Target "Zulib.Build" (fun _ ->

  let files = 
    [| "Zulib/fsharp/Realized/prims.fs";     
      "Zulib/fsharp/Realized/FStar.Pervasives.fs";
      "Zulib/fsharp/Realized/FStar.Mul.fs";     
      "Zulib/fsharp/Realized/FStar.UInt.fs";
      "Zulib/fsharp/Realized/FStar.UInt8.fs";
      "Zulib/fsharp/Realized/FStar.UInt32.fs"; 
      "Zulib/fsharp/Realized/FStar.UInt64.fs";
      "Zulib/fsharp/Realized/FStar.Int.fs";
      "Zulib/fsharp/Realized/FStar.Int64.fs";
      "Zulib/fsharp/Extracted/Zen.Base.fs";
      "Zulib/fsharp/Extracted/Zen.Option.fs";
      "Zulib/fsharp/Extracted/Zen.Tuple.fs";
      //"Zulib/fsharp/Extracted/Zen.Cost.fs";
      "Zulib/fsharp/Realized/Zen.Cost.Realized.fs";
      "Zulib/fsharp/Extracted/Zen.Cost.Extracted.fs";
      "Zulib/fsharp/Extracted/Zen.OptionT.fs";
      "Zulib/fsharp/Extracted/Zen.TupleT.fs";
      "Zulib/fsharp/Extracted/Zen.Vector.fs";
      "Zulib/fsharp/Realized/Zen.Array.Realized.fs";
      "Zulib/fsharp/Extracted/Zen.Array.Extracted.fs";
      //"Zulib/fsharp/Extracted/Zen.Array.fs";
      "Zulib/fsharp/Realized/Zen.Crypto.fs"; 
      "Zulib/fsharp/Realized/Zen.Types.Realized.fs";
      "Zulib/fsharp/Extracted/Zen.Types.Extracted.fs";
      //"Zulib/fsharp/Extracted/Zen.Types.fs" 
    |]    

  let checker = FSharpChecker.Create()

  let compileParams = 
    [|
      "fsc.exe" ; "-o"; "Zulib/bin/Zulib.dll"; "-a";
      "-r"; "packages/FSharp.Compatibility.OCaml/lib/net40/FSharp.Compatibility.OCaml.dll"
      "-r"; "packages/libsodium-net/lib/Net40/Sodium.dll"
    |]

  let messages, exitCode = 
    Async.RunSynchronously (checker.Compile (Array.append compileParams files))

  if exitCode <> 0 then
    let errors = Array.filter (fun (msg:FSharpErrorInfo) -> msg.Severity = FSharpErrorSeverity.Error) messages    
    printfn "%A" errors
    failwith "building Zulib failed"    
    )

Target "Zulib.Pack" (fun _ -> 

  let setParams (p:Fake.Paket.PaketPackParams) = 
    {p with 
      OutputPath = "./Release/"
      TemplateFile = "./Zulib/paket.template"; }

  Paket.Pack setParams    
)

Target "Zulib" (fun _ -> ())

"Zulib.Extract"
  ==> "Zulib.Build"    
  ==> "Zulib.Pack"    
  ==> "Zulib"
  
RunTargetOrDefault "Zulib"
