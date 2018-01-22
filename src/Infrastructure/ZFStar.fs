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
            try 
                let assembly = System.Reflection.Assembly.LoadFrom (assemblyPath)
                Ok assembly
            with 
            | ex -> Error ex.Message                                   
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

let private extract code moduleName =
    let oDir = Platform.normalizeNameToFileSystem (Path.GetTempPath()) / Path.GetRandomFileName()
    try
        Directory.CreateDirectory oDir |> ignore
        let fn = oDir / Platform.normalizeNameToFileSystem moduleName
        let fn'original = changeExtention ".orig.fst" fn
        let fn'elabed = changeExtention ".fst" fn
        let fn'extracted = changeExtention ".fs" fn

        File.WriteAllText(fn'original, sprintf "module %s\n%s" moduleName code)
        elaborate fn'original fn'elabed
        |> Result.bind (fun _ ->
            Platform.run "fstar.exe"
                ["--smt"; Platform.workingDirectory / (Platform.getExeSuffix "z3")
                 "--codegen"; "FSharp"
                 "--prims"; "zulib" / "prims.fst"
                 "--extract_module"; moduleName
                 "--include"; "zulib"
                 "--no_default_includes"; fn'elabed;
                 "--odir"; oDir ]
            |> Result.mapError (fun _ -> "extract")
            |> Result.bind (fun _ -> File.ReadAllText fn'extracted |> Ok))
    finally
        Directory.Delete (oDir, true)

let compile path code moduleName = 
    let moduleName = ("Z" + moduleName) 

    extract code moduleName
    |> Result.bind (compile' path moduleName)
    
let load path moduleName = 
    let moduleName = ("Z" + moduleName) 
    let assemblyPath = Path.Combine(path, sprintf "%s.dll" moduleName)
    
    try 
        let assembly = System.Reflection.Assembly.LoadFrom (assemblyPath)
        Ok assembly
    with 
    | ex -> Error ex.Message