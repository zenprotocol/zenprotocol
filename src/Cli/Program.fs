open Argu
open System
open System.IO
open FSharp.Data
open Api.Types

type ImportArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] Import_Arguments of words:string list
    interface IArgParserTemplate with
        member arg.Usage = ""

type PaginationArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] Pagination_Arguments of skip:int * take:int
    interface IArgParserTemplate with
        member arg.Usage = ""

type SendArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] Send_Arguments of asset:string * amount:int64 * address:string * password:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type ActivateContractArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] ActivateContract_Arguments of file:string * numberOfBlocks:int * password:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type ExtendContractArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] ExtendContract_Arguments of address:string * numberOfBlocks:int * password:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type ExecuteContractArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] ExecuteContract_Arguments of address:string * command:string * messageBody:string * asset:string * amount:int64 * password:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type PublishBlockArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] PublishBlock_Arguments of block:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type PasswordArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] Password_Arguments of password:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type NoArgs =
    | [<Hidden>] NoArg
    interface IArgParserTemplate with
        member arg.Usage = "get address"

type Arguments =
    | [<UniqueAttribute>] Port of port:uint16
#if DEBUG
    | [<AltCommandLine("-l1")>] Local1
    | [<AltCommandLine("-l2")>] Local2
#endif
    | [<CliPrefix(CliPrefix.None)>] Balance of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] History of ParseResults<PaginationArgs>
    | [<CliPrefix(CliPrefix.None)>] Address of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] Resync of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] Import of ParseResults<ImportArgs>
    | [<CliPrefix(CliPrefix.None)>] Send of ParseResults<SendArgs>
    | [<CliPrefix(CliPrefix.None)>] Activate of ParseResults<ActivateContractArgs>
    | [<CliPrefix(CliPrefix.None)>] Extend of ParseResults<ExtendContractArgs>
    | [<CliPrefix(CliPrefix.None)>] Execute of ParseResults<ExecuteContractArgs>
    | [<CliPrefix(CliPrefix.None)>] Active of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] PublishBlock of ParseResults<PublishBlockArgs>
    | [<CliPrefix(CliPrefix.None)>] AccountExists of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] CheckPassword of ParseResults<PasswordArgs>
    | [<CliPrefix(CliPrefix.None)>] MnemonicPhrase of ParseResults<PasswordArgs>
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Port _ -> "port of zen-node API"
#if DEBUG
            | Local1 -> "use port of local1 testing node"
            | Local2 -> "use port of local2 testing node"
#endif
            | Balance _ -> "get wallet balance"
            | History _ -> "list wallet transactions"
            | Address _ -> "get wallet address"
            | Resync _ -> "resync wallet"
            | Import _ -> "import wallet seed from mnemonic sentence. \n First word is the password and the rest are the mnemonic phrase"
            | Send _ -> "send asset to address"
            | Activate _ -> "activate contract"
            | Extend _ -> "extend contract activation"
            | Execute _ -> "execute contract"
            | Active _ -> "list active contracts"
            | PublishBlock _ -> "publish a block to the network"
            | AccountExists _ -> "check for an existing account"
            | CheckPassword _ -> "check a password"
            | MnemonicPhrase _ -> "get the mnemonic phrase"

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "zen-ctl", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    let mutable port = 31567us

    List.iter (fun arg ->
        match arg with
        | Port p -> port <- p
#if DEBUG
        | Local1 -> port <- 36000us
        | Local2 -> port <- 36001us
#endif
        | _ -> ()
        ) (results.GetAllResults())

    let getUri =
        sprintf "http://127.0.0.1:%d/%s" port

    let exit error = (errorHandler :> IExiter).Exit(error, ErrorCode.AppSettings (*=1*))

    let getResponse (response : HttpResponse) =
        let text =
            match response.Body with
            | Text text -> text
            | Binary bytes -> Text.Encoding.ASCII.GetString bytes

        if response.StatusCode <> 200 then
            exit text
        else
            text

    let printResponse = getResponse >> printfn "%s"

    try
        match results.TryGetSubCommand() with
        | Some (Send args) ->
            let asset, amount, address, password = args.GetResult <@ Send_Arguments @>
            "wallet/send"
            |> getUri
            |> (new SendRequestJson.Root(address, asset, amount, password))
                .JsonValue.Request
            |> printResponse
        | Some (Balance _) ->
            let balance =
                "wallet/balance"
                |> getUri
                |> BalanceResponseJson.Load

            printfn "Asset\t\t| Balance"
            printfn "============================"

            Array.iter (fun (assertBalance:BalanceResponseJson.Root) ->
                printfn " %s\t| %d" assertBalance.Asset assertBalance.Balance) balance
        | Some (History args) ->
            let skip, take = args.GetResult <@ Pagination_Arguments @>
            let transactions =
                sprintf "wallet/transactions?skip=%d&take=%d" skip take
                |> getUri
                |> TransactionsResponseJson.Load

            printfn "TxHash\t| Asset\t| Amount\t|Confirmations"
            printfn "==================================================="

            Array.iter (fun (entry:TransactionsResponseJson.Root) ->
                printfn "%s\t%s\t%d\t%d" entry.TxHash entry.Asset entry.Amount entry.Confirmations
            ) transactions
        | Some (Address _) ->
            let result =
                "wallet/address"
                |> getUri
                |> Http.Request
                |> getResponse
                |> JsonValue.String
            printfn "%s" (result.AsString())
        | Some (Resync _) ->
            "wallet/resync"
            |> getUri
            |> Http.Request
            |> printResponse
        | Some (Import args) ->
            match args.GetResult <@ Import_Arguments @> with
            | [_]
            | [] -> printfn "Must include a password and list of words"
            | password :: words ->
                "wallet/import"
                |> getUri
                |> (new ImportSeedJson.Root(password, List.toArray words)).JsonValue.Request
                |> printResponse

        | Some (Activate args) ->
            let file, numberOfBlocks, password = args.GetResult <@ ActivateContract_Arguments @>

            match File.Exists file with
                | false ->
                    printfn "File not found: %s" file
                | true ->
                    let code = File.ReadAllText file
                    let result =
                        "wallet/contract/activate"
                        |> getUri
                        |> (new ContractActivateRequestJson.Root(code, numberOfBlocks, password))
                            .JsonValue.Request
                        |> getResponse
                        |> ContractActivateResponseJson.Parse

                    printfn "Contract activated.\nAddress: %s\nContract Id: %s" result.Address result.ContractId
        | Some (Extend args) ->
            let address, numberOfBlocks, password = args.GetResult <@ ExtendContract_Arguments @>
            "wallet/contract/extend"
            |> getUri
            |> (new ContractExtendRequestJson.Root(
                    address, numberOfBlocks, password))
                .JsonValue.Request
            |> printResponse
        | Some (Execute args) ->
            let address, command, messageBody, asset, amount, password =
                args.GetResult <@ ExecuteContract_Arguments @>

            let messageBody = if messageBody = "None" || messageBody = "none" || messageBody = "null" then "" else messageBody

            "wallet/contract/execute"
            |> getUri
            |> (new ContractExecuteRequestJson.Root(
                    address, command, messageBody,
                    new ContractExecuteRequestJson.Options(true, ""),
                        [| new ContractExecuteRequestJson.Spend(asset, amount) |],
                        password))
                .JsonValue.Request
            |> printResponse
        | Some (Active _) ->
            let activeContracts =
                "contract/active"
                |> getUri
                |> ActiveContractsResponseJson.Load

            printfn "Address\t\t| Contract Id\t\t | Expire"
            printfn "========================================="

            Array.iter (fun (activeContract:ActiveContractsResponseJson.Root) ->
                printfn " %s %s\t| %d" activeContract.Address activeContract.ContractId activeContract.Expire) activeContracts
        | Some (PublishBlock args) ->
            let block = args.GetResult <@ PublishBlock_Arguments @>
            "blockchain/publishblock"
            |> getUri
            |> (new PublishBlockJson.Root(block))
                .JsonValue.Request
            |> printResponse
        | Some (AccountExists _) ->
            "wallet/exists"
            |> getUri
            |> Http.Request
            |> printResponse
        | Some (CheckPassword args) ->
            let password = args.GetResult <@ Password_Arguments @>
            "wallet/checkpassword"
            |> getUri
            |> (new CheckPasswordJson.Root(password))
                .JsonValue.Request
            |> printResponse
        | Some (MnemonicPhrase args) ->
            let password = args.GetResult <@ Password_Arguments @>
            "wallet/mnemonicphrase"
            |> getUri
            |> (new CheckPasswordJson.Root(password))
                .JsonValue.Request
            |> printResponse
        | _ -> ()
    with
    | :? Net.WebException as ex ->
        use reader = new StreamReader(ex.Response.GetResponseStream())
        exit (reader.ReadToEnd())
    | :? AggregateException as ex ->
        exit (ex.Flatten().InnerException.Message)
    | ex ->
        exit ex.Message

    0
