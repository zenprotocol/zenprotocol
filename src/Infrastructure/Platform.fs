module Platform

open System
open System.IO
open System.Text
open System.Diagnostics
open System.Runtime.InteropServices

[<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true )>]
extern uint16 GetShortPathName(
    string lpszLongPath,
    StringBuilder lpszShortPath,
    uint16 cchBuffer)

let private getPlatform =
    let platform = Environment.OSVersion.Platform
    if (platform = PlatformID.Unix 
        && Directory.Exists "/Applications"
        && Directory.Exists "/System"
        && Directory.Exists "/Users"
        && Directory.Exists "/Volumes")
            then PlatformID.MacOSX
    else platform

let private isUnix =
    match getPlatform with
        | PlatformID.Unix
        | PlatformID.MacOSX ->
            true
        | _ ->
            false

let normalizeNameToFileSystem = 
    if isUnix then
        id
    else
        fun fileName ->
            let bufferSize = uint16 256
            let shortNameBuffer = new StringBuilder((int)bufferSize)
            GetShortPathName(fileName, shortNameBuffer, bufferSize) |> ignore
            shortNameBuffer.ToString()

let workingDirectory = 
    Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    |> normalizeNameToFileSystem

let getFrameworkPath =
    match getPlatform with
        | PlatformID.Unix -> "/usr/lib/mono/4.6.2-api/"
        | PlatformID.MacOSX -> "/Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.6.2-api/"
        | _ -> @"C:\Windows\Microsoft.NET\Framework\v4.0.30319\"

let private monoM =
    if isUnix then 
        List.tryFind File.Exists //TODO: prioritize
            ["/usr/bin/mono"
             "/usr/local/bin/mono"
             "/Library/Frameworks/Mono.framework/Versions/Current/Commands/mono"]
    else
        Some "unused"

let private mono = 
    match monoM with
        | Some mono -> mono
        | _ -> 
            "Cannot find mono"
            |> Exception
            |> raise

let z3 =
    "z3" + if isUnix then "" else ".exe"

let run exe args =
    let exe, args = if isUnix then mono, exe :: args else exe, args
    let pinfo = new ProcessStartInfo(exe, String.concat " " args) 
    pinfo.UseShellExecute <- false
    pinfo.RedirectStandardOutput <- true
    pinfo.RedirectStandardError <- true
    //let output = new StringBuilder()
    let error = new StringBuilder()
    let p = Process.Start(pinfo) 
    //p.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
    p.ErrorDataReceived.Add(fun args -> error.Append(args.Data) |> ignore)
    if not (p.Start()) then
        Error ""
    else
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()
        if p.ExitCode = 0 then
            Ok ()
        else
            Error ("run: " + error.ToString())