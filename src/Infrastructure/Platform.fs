module Infrastructure.Platform

open System
open System.IO
open System.Text
open System.Diagnostics
open System.Runtime.InteropServices
open Exception
open Logary.Message

[<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true )>]
extern uint16 GetShortPathName(
    string lpszLongPath,
    StringBuilder lpszShortPath,
    uint16 cchBuffer)

let platform =
    let platform = Environment.OSVersion.Platform
    if (platform = PlatformID.Unix
        && Directory.Exists "/Applications"
        && Directory.Exists "/System"
        && Directory.Exists "/Users"
        && Directory.Exists "/Volumes")
            then PlatformID.MacOSX
    else platform

let private isUnix =
    match platform with
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

let combine a b = Path.Combine (a,b)

let getFrameworkPath =
    match platform with
    | PlatformID.Unix -> "/usr/lib/mono/4.7-api/"
    | PlatformID.MacOSX -> "/Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.7-api/"
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
    | _ -> failwith "Cannot find mono"

let getExeSuffix =
    (+) (if isUnix then "" else ".exe")

let run exe args =
    let exe, args = if isUnix then mono, exe :: args else exe, args
    let p = new Process();
    p.StartInfo <-
        new ProcessStartInfo(
            FileName = exe,
            Arguments = String.concat " " args,
            WorkingDirectory = workingDirectory,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false)
    let appender (sb : StringBuilder) =
        let (+) (sb : StringBuilder) (s : string) = sb.Append s |> ignore
        fun (args : DataReceivedEventArgs) ->
            let data = args.Data
            if not (String.IsNullOrWhiteSpace data) then
                if sb.Length <> 0 then
                    sb + Environment.NewLine
                sb + data
    let error = new StringBuilder()
    p.ErrorDataReceived.Add(appender error)
    let output = new StringBuilder()
    p.OutputDataReceived.Add(appender output)
    try
        if p.Start() then
            p.BeginErrorReadLine()
            p.BeginOutputReadLine()
            p.WaitForExit()
            if p.ExitCode = 0 then
                Ok ()
            else
                let error = error.ToString()
                if error.Length > 0 then
                    eventX "{error}"
                    >> setField "error" error
                    |> Log.info
                let output = output.ToString()
                if output.Length > 0 then
                    eventX "{output}"
                    >> setField "output" error
                    |> Log.verbose
                Error error
        else
            Error "failed to start process"
    with _ as ex ->
        Exception.toError "run" ex

let removeDirectory path =
    if Directory.Exists path then
        Directory.Delete(path, true)

let cleanDirectory path =
    removeDirectory path

    Directory.CreateDirectory path |> ignore