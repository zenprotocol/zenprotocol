module ZFStar

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let private fsChecker = FSharpChecker.Create()
let private changeExtention extention path = Path.ChangeExtension (path, extention)

let private getMethodInfo code =
    //let code = code + """nopen MBrace.FsPickler.Combinators
    //let pickler = Pickler.auto<ZFStar.Contract>
    //let pickled = Binary.pickle pickler mainFunction"""
    try
        let fn = Path.GetTempFileName()
        let fni = changeExtention ".fs" fn
        let fno = changeExtention ".dll" fn
        File.WriteAllText(fni, code)
        let (/) a b = Path.Combine (a,b)
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
                    //.GetModules().[0]
                    //.GetTypes().[0]
                    //.GetProperty("pickled")
                    //.GetValue(null) :?> byte[]
        else
            errors
            |> Array.map (fun e -> e.ToString()) 
            |> Array.append [|"compile:"|]
            |> String.concat " "
            |> Error
    with _ as ex ->
        Error ("compile exception: " + ex.Message)

let private extract moduleName code =
    let code = sprintf "module %s\n%s" moduleName code
    let (/) a b = Path.Combine (a,b)
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
            File.WriteAllText(fn'original, code)
            //IOUtils.elaborate fn'orig fn'elabed
            let (+/) = (/) Platform.workingDirectory
            let (>=>) a b = Result.bind b a
            Platform.run ((+/) "fstar.exe")
                ["--lax"; //"--smt"; Platform.z3;
                 "--codegen"; "FSharp";
                 "--prims"; (+/) "zulib" / "prims.fst";
                 "--extract_module"; moduleName;
                 "--include"; (+/) "zulib";
                 "--no_default_includes"; fn'original //fn'elabed;               
                 "--odir"; oDir ]
            >=> 
            fun _ -> File.ReadAllText fn'extracted |> Ok
        with _ as ex ->
            Error ("extract: " + ex.Message)
    finally
        Directory.Delete (oDir, true)

let compile moduleName code = 
    let (>=>) a b = Result.bind b a

    extract moduleName code
    >=>
    getMethodInfo