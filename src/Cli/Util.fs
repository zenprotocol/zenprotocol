module Cli.Util

open Argu
open System
open FSharp.Data

let getUri' = sprintf "http://127.0.0.1:%s/%s"
 
let split value seperators =
    (value:String).Split seperators
    |> Array.choose (fun value ->
        if String.IsNullOrWhiteSpace value then
            None
        else
            Some value
    )


let port = sprintf "2000%d"

let getURL = port >> getUri'

let errorHandler =
    let colorizer = function | ErrorCode.HelpText -> None
                             | _ -> Some ConsoleColor.Red
    ProcessExiter colorizer

let exit (errorMsg: string): 'a =
    (errorHandler :> IExiter).Exit(errorMsg, ErrorCode.AppSettings (*=1*))

let getResponse ({Body=body;StatusCode=statusCode}: HttpResponse) =
        let text = match body with
                   | Text text -> text
                   | Binary bytes -> Text.Encoding.ASCII.GetString bytes

        if statusCode <> 200 then exit text else text

let printResponse = printfn "%s"

let checker msg = msg = "None" || msg = "none" || msg = "null"

let unwrapString string =
    let endString = (String.length string) - 2
    string.[1..endString]