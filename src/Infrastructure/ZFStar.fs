module Infrastructure.ZFStar

open System
open System.Reflection
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Exception
open Infrastructure.Result
open Logary.Message
open FSharpx.Functional.Prelude

let private fsChecker = FSharpChecker.Create()
let changeExtention extention path = Path.ChangeExtension (path, extention)

let private (/) a b = Path.Combine (a,b)

let private error s =
    sprintf "%s: %s" s

let compatibilityPath = 
    (Platform.workingDirectory, "compatibility", "v0_contract_hints")
    |> Path.Combine

let private compile' path moduleName code =
    try
        let tempFileName = Path.GetTempFileName()
        let codeFileName = changeExtention ".fs" tempFileName
        let assemblyPath = Path.Combine(path, sprintf "%s.dll" moduleName)

        if File.Exists assemblyPath then
            Ok ()
        else
            File.WriteAllText(codeFileName, code)
            let (+/) = (/) Platform.getFrameworkPath

            let errors, exitCode =
                Async.RunSynchronously
                <| fsChecker.Compile(
                    [|"fsc.exe";
                    "--noframework";
                    "--mlcompatibility";
                    "-o"; assemblyPath;
                    "-a"; codeFileName;
                    "-r"; (+/) "mscorlib.dll";
                    "-r"; (+/) "System.Core.dll";
                    "-r"; (+/) "System.dll";
                    "-r"; (+/) "System.Numerics.dll";
                    "-r"; "Zulib.dll";
                    "-r"; "FSharp.Compatibility.OCaml.dll";
                    "-r"; "BouncyCastle.Crypto.dll";
                    "-r"; "FSharpx.Collections.dll";
                    "-r"; "FsBech32.dll" |])
            if exitCode = 0 then
                Ok ()
            else
                errors
                |> Array.map (fun e -> e.ToString())
                |> String.concat " "
                |> error "compile"
                |> Error
    with _ as ex ->
        Exception.toError "compile" ex

let private elaborate input_filepath output_target =
    try
        ASTUtils.parse_file input_filepath
        |>! ASTUtils.check_ast
        |>  ASTUtils.elab_ast
        |>  ASTUtils.add_main_to_ast
        |>  ASTUtils.write_ast_to_file output_target
        Ok ()
    with _ as ex ->
        eventX "elaboration error: {ex}"
        >> setField "ex" (sprintf "%A" ex)
        |> Log.info
        Error "elaborate"

let z3Name =
    match Platform.platform with
    | PlatformID.MacOSX -> "z3-osx"
    | PlatformID.Unix -> "z3-linux"
    | _ -> "z3.exe"

let private fstar outDir inputFile args =
    [ "--smt"; z3Name
      "--prims"; "zulib" / "prims.fst"
      "--include"; "zulib"
      "--no_default_includes"; inputFile;
      "--odir"; outDir ] @ args
    |> Platform.run "fstar.exe"

let private wrapFStar errorResult outputFile fstarFn =
    fstarFn
    |> Result.mapError (fun error ->
        sprintf "%A\n%A" errorResult error)
    |> Result.map (fun _ -> File.ReadAllText outputFile)

#if DEBUG
let mutable unitTesting = false
#endif

let initOutputDir moduleName =

#if DEBUG
    let oDir =
        if unitTesting then
            Platform.workingDirectory / "../../test-contracts"
            |> Platform.normalizeNameToFileSystem
        else
            Platform.normalizeNameToFileSystem compatibilityPath
#else
    let oDir = Platform.normalizeNameToFileSystem (Path.GetTempPath()) / Path.GetRandomFileName()
#endif

    Directory.CreateDirectory oDir |> ignore
    oDir, oDir / moduleName

let private extract code hints limits rlimit moduleName =
    let oDir, file = initOutputDir moduleName
    let originalFile = changeExtention ".orig.fst" file
    let hintsFile = changeExtention ".fst.hints" file
    let elaboratedFile = changeExtention ".fst" file
    let extractedFile = changeExtention ".fs" file

#if DEBUG
    if unitTesting && File.Exists extractedFile then
        File.ReadAllText extractedFile
        |> Ok
    else
#endif

    File.WriteAllText(originalFile, sprintf "module %s\n%s" moduleName code)
    File.WriteAllText(hintsFile, hints)

    try
        let maxFuel, maxIFuel = limits
        elaborate originalFile elaboratedFile
        |> Result.bind (fun _ ->
            fstar oDir elaboratedFile [
                "--codegen"; "FSharp"
                "--use_hints"
                "--strict_hints"
                "--use_cached_modules"
                "--extract_module"; moduleName
                "--max_fuel"; maxFuel.ToString()
                "--max_ifuel"; maxIFuel.ToString()
                "--z3rlimit"; rlimit.ToString()
                ]
            |> wrapFStar "extract" extractedFile)
    finally
#if DEBUG
        eventX "extract output directory: {dir}"
        >> setField "dir" oDir
        |> Log.debug
#else
        Directory.Delete (oDir, true)
#endif

let calculateMetrics hints =
    let file = Path.GetTempFileName()
    let hintsFile = changeExtention ".fst.hints" file

    File.WriteAllText(hintsFile, hints)

    try
        try
            Hints.read_hints hintsFile
            |> Option.map (fun hintsMap ->
                let getFuel hint = int (hint : Hints.Hint).fuel
                let getIFuel hint = int (hint : Hints.Hint).ifuel
                let getMax getterFn hintsMap =
                    hintsMap
                    |> Map.toSeq
                    |> Seq.map snd
                    |> Seq.map (fun hints ->
                        List.map getterFn hints
                        |> List.max)
                    |> Seq.max
                (getMax getFuel hintsMap, getMax getIFuel hintsMap))
            |> function
            | Some limits -> Ok limits
            | None -> Error "limits"
        with _ as ex ->
            Exception.toError "limits" ex
    finally
#if DEBUG
        eventX "calculate metrics output: {file}"
        >> setField "file" file
        |> Log.debug
#else
        ()
#endif

let recordHints rlimit code moduleName =
    let oDir, file = initOutputDir moduleName
    let originalFile = changeExtention ".orig.fst" file
    let hintsFile = oDir / changeExtention ".fst.hints" file
    let elaboratedFile = changeExtention ".fst" file

#if DEBUG
    if unitTesting && File.Exists hintsFile then
        File.ReadAllText hintsFile
        |> Ok
    else
#endif

    File.WriteAllText(originalFile, sprintf "module %s\n%s" moduleName code)

    try
        elaborate originalFile elaboratedFile
        |> Result.bind (fun _ ->
            fstar oDir elaboratedFile [
                 "--z3rlimit";(string rlimit)
                 "--use_cached_modules"
                 "--record_hints"
                 "--use_hints"
                 ]
            |> wrapFStar "record hints" hintsFile)
    finally
#if DEBUG
        eventX "record hints output directory: {dir}"
        >> setField "dir" oDir
        |> Log.debug
#else
        Directory.Delete (oDir, true)
#endif


let compile path code hints rlimit moduleName =
    calculateMetrics hints
    |> Result.bind (fun limits -> extract code hints limits rlimit moduleName)
    |> Result.bind (compile' path moduleName)

let load path moduleName =
    let assemblyPath = Path.Combine(path, sprintf "%s.dll" moduleName)

    if File.Exists assemblyPath then
        try
            assemblyPath
            |> Assembly.LoadFrom
            |> Ok
        with _ as ex ->
            Error ex.Message
    else
        Error "compiled contract DLL file not found"

let totalQueries hints =
    let file = Path.GetTempFileName()
    let hintsFile = changeExtention ".fst.hints" file

    File.WriteAllText(hintsFile, hints)

    try (
        try
            Hints.read_hints hintsFile
            |> function
            | Some hints -> Ok hints
            | None -> Error "total queries: could not read hints"
        with _ ->
            Error "total queries: invalid hints")
        <@> ( Hints.total_num_queries
              >> uint32 )
    finally
#if DEBUG
        eventX "total queries output : {file}"
        >> setField "file" file
        |> Log.debug
#else
        ()
#endif