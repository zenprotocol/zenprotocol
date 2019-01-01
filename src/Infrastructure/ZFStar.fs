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
let private changeExtention extention path = Path.ChangeExtension (path, extention)

let private (/) a b = Path.Combine (a,b)

let private error s =
    sprintf "%s: %s" s

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
            Platform.normalizeNameToFileSystem (Path.GetTempPath()) / Path.GetRandomFileName()
#else
    let oDir = Platform.normalizeNameToFileSystem (Path.GetTempPath()) / Path.GetRandomFileName()
#endif

    Directory.CreateDirectory oDir |> ignore
    oDir, oDir / moduleName


let compatibilityFiles: Set<string> = Set.ofList [
    "Zca055cc0af4d25ea1c8bbbf41444aadd68a168558397516b2f64727d87e72f97"
    "Z112c1053b7713506aa8f00d643b19b1a67f29e345f9be6c8f3ad569e57426c86"
    "Z60c9dc79adf6d6d5caf812a18e87e31b757ea2ebca4dba385ef8e3fc220760a6"
    "Z108782fb49e44169ac3c3307841457e6332e2372cdccaa4ede6b41b57265d18e"
    "Z4edb5daa76c613abbfa3294399f63490dafeef38d7284e5ef34f15f8d1e488b7"
    "Za5094510fe0377146e07c34f7abc3109e81c05d62cd350ec00ca8c83e8926a72"

    "Zf24db32aa1881956646d3ccbb647df71455de10cf98b635810e8870906a56b63"
    "Zc59702cbb376b1ec097ba870eaa6e7eb304bc5889b7dcc7fa96e1fe3ef193f24"
    "Zcfcfe6bba6775dd01b3b11f0d2b03b134ed678b75468d221866bf030f679118a"
    "Z4a6784d03d556eb3b8051a16b0d84cb103f1de3c70e28ac2ef46c9cec5fc8ddd"
    "Zd1951c95130928b1362db63cf712b99923cfe581f646af0a25c89bdadf4b2c8d"
    "Z48ca75a61f391e77fb3142cb96d7bfd91f9c0151ff66b2a46c1ecce0033f066b"
    "Z2f7ca11c62a0b30f3c57b1e37c1d4e13af5a876995dcc5c12a25bcad8058fee0"
    "Zda0181b484e8e516dd288f4bcf3ea2db195452586de5c0364cfa0e8f4a732a8f"
    "Z30b0e9de06f6b39efb3c4817e598fcab7de46f88bfdd2f3592fc0dca7f00f12f"
    "Z2bd5c8f08ea6d7546eb892e535950fb1946a1af4d2765b9d58b9f96f8faaf760"
    "Z12c3cbce97fbcea65cc1b89cbc9cb26566401e5f7b54d045648818c1234b4ee7"
    "Z8a5f28e5d9175866b8b2f3e296400794b226e13572a73f95ed2461ac943b283d"
    "Z5b8fbf5f70a8d7b46f2601794d2b16f1d9c009de526a80ffc44266b4c9fddc7e"
    "Zd620b00ab006aece5d1eb6b390d9af9801943e41954f320510804beb7e67ef23"
    "Zc2486d73b824f4412a819171d9e9f576f0c00fcdd1701759b9d54b1f22ffecfc"
    "Zc94b79e9393cbdff85861706fdbc9048adef4eef888c509269ecc26afe7637da"
    "Z504c9782738628e03ef347f9f094b214cea0fa12d49e3927743a849caaeb09e2"
]

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
                begin if Set.contains moduleName compatibilityFiles
                      then "--lax"
                      else "" end
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

let recordHints code moduleName =
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
                 "--z3rlimit";(2723280u * 4u).ToString()
                 "--use_cached_modules"
                 "--use_hints"
                 "--record_hints"
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