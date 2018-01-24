open Argu
open System
open FSharp.Data
open Api.Types

type SpendArgs = 
    | [<MainCommand("COMMAND");ExactlyOnce>] Spend_Arguments of asset:string * amount:int64 * address:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type ActivateContractArgs = 
    | [<MainCommand("COMMAND");ExactlyOnce>] ActivateContract_Arguments of file:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type ExecuteContractArgs = 
    | [<MainCommand("COMMAND");ExactlyOnce>] ExecuteContract_Arguments of address:string * command:string * asset:string * amount:int64 
    interface IArgParserTemplate with
        member arg.Usage = ""
                                                                             
type NoArgs = 
    | [<Hidden>] NoArg
    interface IArgParserTemplate with
        member arg.Usage = "get address"
            
type Arguments =
    | [<UniqueAttribute>] Port of port:uint16
    | [<AltCommandLine("-l1")>] Local1 
    | [<AltCommandLine("-l2")>] Local2    
    | [<CliPrefix(CliPrefix.None)>] Balance of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] Address of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] Spend of ParseResults<SpendArgs>
    | [<CliPrefix(CliPrefix.None)>] Activate of ParseResults<ActivateContractArgs>
    | [<CliPrefix(CliPrefix.None)>] Execute of ParseResults<ExecuteContractArgs>
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Port _ -> "port of zen-node API"
            | Local1 -> "use port of local1 testing node" 
            | Local2 -> "use port of local2 testing node"
            | Balance _ -> "get wallet balance"
            | Address _ -> "get wallet address"
            | Spend _ -> "send asset to an address" 
            | Activate _ -> "activate contract"
            | Execute _ -> "execute contract"
            

[<EntryPoint>]
let main argv = 
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "zen-ctl", errorHandler = errorHandler)
                
    let results = parser.ParseCommandLine argv
    
    let mutable port = 31567us
        
    List.iter (fun arg -> 
        match arg with
        | Port p -> port <- p
        | Local1 -> port <- 36000us
        | Local2 -> port <- 36001us
        | _ -> ()
        ) (results.GetAllResults())                                                       
       
    printfn ""

    let getUri =
        sprintf "http://127.0.0.1:%d/%s" port
       
    match results.TryGetSubCommand() with
    | Some (Spend args) ->        
        let asset,amount,address = args.GetResult <@ Spend_Arguments @>
        let send = new SpendRequestJson.Root(address, new SpendRequestJson.Spend(asset, amount))        
        
        let response = send.JsonValue.Request (getUri "wallet/spend")
        
        match response.StatusCode, response.Body with
        | 200,_ -> printfn "Success"
        | code, HttpResponseBody.Text text -> printfn "Failed %d %s" code text
        | code,_ -> printfn "Failed %d with binary response" code
    | Some (Balance _) -> 
        let balance = 
            BalanceResponseJson.Load(getUri "wallet/balance")
                
        printfn "Asset\t\t| Balance"
        printfn "============================"
        
        Array.iter (fun (assertBalance:BalanceResponseJson.Root) -> 
            printfn " %s\t| %d" assertBalance.Asset assertBalance.Balance) balance                                                                   
    | Some (Address _) ->
        let address = 
            AddressJson.Load (getUri "wallet/address")                                
               
        printfn "%s" address.Address
    | Some (Activate args) ->
        let file = args.GetResult <@ ActivateContract_Arguments @>

        match System.IO.File.Exists file with
            | false -> 
                printfn "File not found: %s" file
            | true ->
                let code = System.IO.File.ReadAllText file
                let activate = new ContractActivateRequestJson.Root(code)        
        
                let response = activate.JsonValue.Request (getUri "wallet/contract/activate")
        
                match response.StatusCode, response.Body with
                    | 200,_ -> printfn "Success %A" response.Body
                    | code, HttpResponseBody.Text text -> printfn "Failed %d %s" code text
                    | code,_ -> printfn "Failed %d with binary response" code
    | Some (Execute args) ->
        let address,command,asset,amount = args.GetResult <@ ExecuteContract_Arguments @>
        let activate = new ContractExecuteRequestJson.Root(address,command, [| new ContractExecuteRequestJson.Spend(asset, amount) |])

        let response = activate.JsonValue.Request (getUri "wallet/contract/execute")

        match response.StatusCode, response.Body with
            | 200,_ -> printfn "Success"
            | code, HttpResponseBody.Text text -> printfn "Failed %d %s" code text
            | code,_ -> printfn "Failed %d with binary response" code
    | _ -> ()
    
    printfn ""                                              
                
    0 // return an integer exit code
