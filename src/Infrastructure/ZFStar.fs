module Infrastructure.ZFStar

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Exception

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
    [ "--smt"; Platform.workingDirectory / (Platform.getExeSuffix "z3")
      "--prims"; "zulib" / "prims.fst"
      "--include"; "zulib"
      "--no_default_includes"; inputFile;
      "--odir"; outDir ] @ args
    |> Platform.run "fstar.exe"

let private wrapFStar errorResult outputFile fstarFn =
    fstarFn
    |> Result.mapError (fun _ -> errorResult)
    |> Result.map (fun _ -> File.ReadAllText outputFile)

let private initOutputDir moduleName = 
    let oDir = Platform.normalizeNameToFileSystem (Path.GetTempPath()) / Path.GetRandomFileName()
    Directory.CreateDirectory oDir |> ignore
    oDir, oDir / Platform.normalizeNameToFileSystem moduleName

let private extract (code, hints) moduleName = 
    let oDir, file = initOutputDir moduleName
    let originalFile = changeExtention ".orig.fst" file
    let hintsFile = changeExtention ".fst.hints" file
    let elaboratedFile = changeExtention ".fst" file
    let extractedFile = changeExtention ".fs" file

    File.WriteAllText(originalFile, sprintf "module %s\n%s" moduleName code)
    File.WriteAllText(hintsFile, hints)

    try
        elaborate originalFile elaboratedFile
        |> Result.bind (fun _ ->
            fstar oDir elaboratedFile [
                "--use_hints"
                "--codegen"; "FSharp"
                "--extract_module"; moduleName ]
            |> wrapFStar "extract" extractedFile)
    finally
        Directory.Delete (oDir, true)

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
                 "--record_hints"
                 "--no_default_includes"; elaboratedFile ]
            |> wrapFStar "record hints" hintsFile)
    finally
        ()
        Directory.Delete (oDir, true)

let compile path (code,hints) moduleName = 
    extract (code,hints) moduleName
    |> Result.bind (compile' path moduleName)
    
let load path moduleName = 
    try 
        Path.Combine(path, sprintf "%s.dll" moduleName)
        |> System.Reflection.Assembly.LoadFrom 
        |> Ok
    with _ as ex ->
        Error ex.Message