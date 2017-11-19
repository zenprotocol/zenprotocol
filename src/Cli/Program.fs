open Argu
open System
open FSharp.Data
open Api.Types

type SendArgs = 
    | [<MainCommand("COMMAND");ExactlyOnce>] Send_Arguments of asset:string * amount:int64 * address:string
    interface IArgParserTemplate with
        member arg.Usage = ""
                                                             
type NoArgs = 
    | [<Hidden>] NoArg
    interface IArgParserTemplate with
        member arg.Usage = "get address"
            
type Arguments =
    | [<UniqueAttribute>] Port of port:uint16
    | [<CliPrefix(CliPrefix.None)>] Balance of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] Address of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] Send of ParseResults<SendArgs>
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Port _ -> "port of zen-node API" 
            | Balance _ -> "get wallet balance"
            | Address _ -> "get wallet address"
            | Send _ -> "send asset to an address" 

[<EntryPoint>]
let main argv = 
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "zen-ctl", errorHandler = errorHandler)
                
    let results = parser.ParseCommandLine argv
    
    let port = 
        match results.TryGetResult <@ Port @> with
        | Some port -> port
        | None -> 31567us                                               
       
    printfn ""   
       
    match results.TryGetSubCommand() with
    | Some (Send args) ->        
        let asset,amount,address = args.GetResult <@ Send_Arguments @>
        let send = new TransactionSendJson.Root(asset, amount, address)        
        
        let response = send.JsonValue.Request (sprintf  "http://127.0.0.1:%d/wallet/transaction/send" port)
        
        match response.StatusCode, response.Body with
        | 200,_ -> printfn "Success"
        | code, HttpResponseBody.Text text -> printfn "Failed %d %s" code text
        | code,_ -> printfn "Failed %d with binary response" code
    
    | Some (Balance _) -> 
        let balance = 
            BalanceJson.Load(sprintf "http://127.0.0.1:%d/wallet/balance" port)
                
        printfn "Asset\t\t| Balance"
        printfn "============================"
        
        Array.iter (fun (assertBalance:BalanceJson.Root) -> 
            printfn " %s\t| %d" assertBalance.Asset assertBalance.Balance) balance                                                                   
    | Some (Address _) ->
        let address = 
            AddressJson.Load (sprintf "http://127.0.0.1:%d/wallet/address" port)                                
               
        printfn "%s" address.Address
    | _ -> ()
    
    printfn ""                                              
                
    0 // return an integer exit code
