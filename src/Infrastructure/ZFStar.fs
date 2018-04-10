module Infrastructure.ZFStar

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Exception
open Infrastructure.Result

let private fsChecker = FSharpChecker.Create()
let private changeExtention extention path = Path.ChangeExtension (path, extention)

let private (/) a b = Path.Combine (a,b)

let private error s =
    sprintf "%s: %s" s

let private compile' path moduleName code =
    //let code = code + """nopen MBrace.FsPickler.Combinators
    //let pickler = Pickler.auto<ZFStar.Contract>
    //let pickled = Binary.pickle pickler mainFunction"""
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
                    "-r"; "zulib" / "Zulib.dll";
                    //"-r"; "FsPickler.dll";
                    "-r"; "FSharp.Compatibility.OCaml.dll"|])
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
        let ast = FStar.Parser.Driver.parse_file input_filepath
        let elab'd_ast = ASTUtils.elab_ast ast
                         |> ASTUtils.add_main_to_ast
        ASTUtils.write_ast_to_file elab'd_ast output_target
        Ok ()
    with _ as ex ->
        Log.info "elaboration error: %A" ex
        Error "elaborate"

let private fstar outDir inputFile args =
    let z3Name =
        match Platform.platform with
        | PlatformID.MacOSX -> "z3-osx"
        | PlatformID.Unix -> "z3-linux"
        | _ -> "z3.exe"

    [ "--smt"; Platform.workingDirectory / z3Name
      "--prims"; "zulib" / "prims.fst"
      "--include"; "zulib"
      "--no_default_includes"; inputFile;
      "--odir"; outDir ] @ args
    |> Platform.run "fstar.exe"

let private wrapFStar errorResult outputFile fstarFn =
    fstarFn
    |> Result.mapError (fun _ -> errorResult)
    |> Result.map (fun _ -> File.ReadAllText outputFile)

let initOutputDir moduleName =
    let oDir = Platform.normalizeNameToFileSystem (Path.GetTempPath()) / Path.GetRandomFileName()
    Directory.CreateDirectory oDir |> ignore
    oDir, oDir / moduleName

let private extract code hints limits rlimit moduleName =
    let oDir, file = initOutputDir moduleName
    let originalFile = changeExtention ".orig.fst" file
    let hintsFile = changeExtention ".fst.hints" file
    let elaboratedFile = changeExtention ".fst" file
    let extractedFile = changeExtention ".fs" file

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
        printfn "extract output directory: %A" oDir
#else
        Directory.Delete (oDir, true)
#endif

let calculateMetrics hints =
    let oDir, file = initOutputDir "" //as for now, using the filesystem as temporary solution
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
        printfn "calculate metrics output directory: %A" oDir
#else
        Directory.Delete (oDir, true)
#endif

let recordHints code moduleName =
    let oDir, file = initOutputDir moduleName
    let originalFile = changeExtention ".orig.fst" file
    let hintsFile = oDir / changeExtention ".fst.hints" file
    let elaboratedFile = changeExtention ".fst" file

    File.WriteAllText(originalFile, sprintf "module %s\n%s" moduleName code)

    try
        elaborate originalFile elaboratedFile
        |> Result.bind (fun _ ->
            fstar oDir elaboratedFile [
                 "--z3rlimit";(2723280u * 2u).ToString()
                 "--use_cached_modules"
                 "--record_hints"
                 "--no_default_includes"; elaboratedFile ]
            |> wrapFStar "record hints" hintsFile)
    finally
#if DEBUG
        printfn "record hints output directory: %A" oDir
#else
        Directory.Delete (oDir, true)
#endif

let compile path code hints rlimit moduleName =
    calculateMetrics hints
    |> Result.bind (fun limits -> extract code hints limits rlimit moduleName)
    |> Result.bind (compile' path moduleName)

let load path moduleName =
    try
        Path.Combine(path, sprintf "%s.dll" moduleName)
        |> System.Reflection.Assembly.LoadFrom
        |> Ok
    with _ as ex ->
        Error ex.Message

let totalQueries hints =
    let oDir, file = initOutputDir "" //as for now, using the filesystem as temporary solution
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
        printfn "total queries output directory: %A" oDir
#else
        Directory.Delete (oDir, true)
#endif
