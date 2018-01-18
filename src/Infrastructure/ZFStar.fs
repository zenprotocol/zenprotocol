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

let private compile' code =
    //let code = code + """nopen MBrace.FsPickler.Combinators
    //let pickler = Pickler.auto<ZFStar.Contract>
    //let pickled = Binary.pickle pickler mainFunction"""
    try
        let fn = Path.GetTempFileName()
        let fni = changeExtention ".fs" fn
        let fno = changeExtention ".dll" fn
        File.WriteAllText(fni, code)
        let (+/) = (/) Platform.getFrameworkPath
        let errors, exitCode, dynamicAssembly =
            Async.RunSynchronously 
            <| fsChecker.CompileToDynamicAssembly(
                [|"fsc.exe";
                "--noframework";
                "--mlcompatibility";
                "-o"; fno;
                "-a"; fni;
                "-r"; (+/) "mscorlib.dll";
                "-r"; (+/) "System.Core.dll";
                "-r"; (+/) "System.dll";
                "-r"; (+/) "System.Numerics.dll";
                "-r"; "zulib" / "Zulib.dll";
                //"-r"; "FsPickler.dll";
                "-r"; "FSharp.Compatibility.OCaml.dll"|], None) //Some (stdout, stderr))
        if exitCode = 0 then
            match dynamicAssembly with
            | None -> 
                Error ""
            | Some assembly ->
                Ok assembly
        else
            errors
            |> Array.map (fun e -> e.ToString()) 
            |> String.concat " "
            |> error "compile"
            |> Error
    with _ as ex ->
        Exception.toError "compile" ex

let private extract code moduleName =
    let oDir = Platform.normalizeNameToFileSystem (Path.GetTempPath()) / Path.GetRandomFileName()
    try
        try
            Directory.CreateDirectory oDir |> ignore
            let fn = oDir / Platform.normalizeNameToFileSystem moduleName
            let fn'original = 
                fn
                |> changeExtention ".orig"
                |> changeExtention ".fst"
            //let fn'elabed = changeExtention fn ".fst"
            let fn'extracted = changeExtention ".fs" fn
            File.WriteAllText(fn'original, sprintf "module %s\n%s" moduleName code)
            //IOUtils.elaborate fn'orig fn'elabed
            Platform.run "fstar.exe"
                ["--smt"; Platform.workingDirectory / (Platform.getExeSuffix "z3")
                 "--codegen"; "FSharp"
                 "--prims"; "zulib" / "prims.fst"
                 "--extract_module"; moduleName
                 "--include"; "zulib"
                 "--no_default_includes"; fn'original //fn'elabed;
                 "--odir"; oDir ]
            |> Result.mapError (fun _ -> "extract")
            |> Result.bind (fun _ -> File.ReadAllText fn'extracted |> Ok)
        with _ as ex ->
            Exception.toError "extract" ex
    finally
        Directory.Delete (oDir, true)

let compile code moduleName = 
    "Z" + moduleName
    |> extract code
    |> Result.bind compile'