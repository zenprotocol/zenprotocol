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

type PublicKeyArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] PublicKey_Arguments of path:string * password:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type ImportPublicKeyArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] ImportPublicKey_Arguments of publicKey:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type RawTransactionCreateArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] RawTransactionCreate_Arguments of asset:string * amount:int64 * address:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type RawTransactionSignArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] RawTransactionSign_Arguments of password:string * file:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type RawTransactionPublishArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] RawTransactionPublish_Arguments of file:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type NoArgs =
    | [<Hidden>] NoArg
    interface IArgParserTemplate with
        member arg.Usage = "get address"

type Arguments =
    | [<AltCommandLine("-p");UniqueAttribute>] Port of port:uint16
    | [<AltCommandLine("-t")>] Test
#if DEBUG
    | Local of uint16
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
    | [<CliPrefix(CliPrefix.None)>] ExportZenPublicKey of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] ImportZenPublicKey of ParseResults<ImportPublicKeyArgs>
    | [<CliPrefix(CliPrefix.None)>] RemoveWallet of ParseResults<PasswordArgs>
    | [<CliPrefix(CliPrefix.None)>] PublicKey of ParseResults<PublicKeyArgs>
    | [<CliPrefix(CliPrefix.None)>] RawTx_Create of ParseResults<RawTransactionCreateArgs>
    | [<CliPrefix(CliPrefix.None)>] RawTx_Sign of ParseResults<RawTransactionSignArgs>
    | [<CliPrefix(CliPrefix.None)>] RawTx_Publish of ParseResults<RawTransactionPublishArgs>
    | [<CliPrefix(CliPrefix.None)>] Wallet_Create of ParseResults<PasswordArgs>

    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Port _ -> "port of zen-node API"
            | Test _ -> "use testnet port"
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
            | ExportZenPublicKey _ -> "export zen extended public key"
            | ImportZenPublicKey _ -> "import zen extended public key and create watch-only account"
            | RemoveWallet _ -> "remove wallet"
            | PublicKey _ -> "derive a public key from a given derivation path"
            | RawTx_Create _ -> "create a raw transaction that pass the asset and amount to the address"
            | RawTx_Sign _ -> "sign all possible inputs of the raw transaction and return the signed transaction"
            | RawTx_Publish _ -> "publish a fully signed raw transaction"
            | Wallet_Create _ -> "Generate new mnemonic phrase and creating a wallet. Use mnemonichhrase command to get the newly generated mnemonic phrase."
            | _ -> ""

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "zen-cli.exe", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    let mutable port = 11567us

    List.iter (fun arg ->
        match arg with
        | Port p -> port <- p
        | Test -> port <- 31567us
#if DEBUG
        | Local idx -> port <- 20000us + idx
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

    let saveRawToFile (response : HttpResponse) =
        let text =
            match response.Body with
            | Text text -> text
            | Binary bytes -> Text.Encoding.ASCII.GetString bytes

        if response.StatusCode <> 200 then
            exit text |> printfn "%s"
        else
            let json = RawTransactionResultJson.Parse text

            System.IO.File.WriteAllText(sprintf "%s.raw" json.TxHash, json.Tx)

            printfn "Raw transaction written to file %s.raw" json.TxHash

    try
        match results.TryGetSubCommand() with
        | Some (Send args) ->
            let asset, amount, address, password = args.GetResult <@ Send_Arguments @>
            "wallet/send"
            |> getUri
            |> (new SendRequestJson.Root([| new SendRequestJson.Output(address, asset, amount) |], password))
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
        | Some (ImportZenPublicKey args) ->
            let publicKey = args.GetResult <@ ImportPublicKey_Arguments @>
            "wallet/importzenpublickey"
            |> getUri
            |> (new ImportZenPublicKey.Root(publicKey)).JsonValue.Request
            |> printResponse
        | Some (ExportZenPublicKey _) ->
            "wallet/zenpublickey"
            |> getUri
            |> Http.Request
            |> printResponse
        | Some (RemoveWallet args) ->
            let password = args.GetResult <@ Password_Arguments @>

            "wallet/remove"
            |> getUri
            |> (new CheckPasswordJson.Root(password))
                .JsonValue.Request
            |> printResponse
        | Some (PublicKey args) ->
            let path, password = args.GetResult <@ PublicKey_Arguments @>
            "wallet/publickey"
            |> getUri
            |> (new GetPublicKeyJson.Root(path, password))
                .JsonValue.Request
            |> printResponse
        | Some (RawTx_Create args) ->
            let asset, amount, address = args.GetResult <@ RawTransactionCreate_Arguments @>
            "wallet/rawtransaction/create"
            |> getUri
            |> (new CreateRawTransactionJson.Root([| new CreateRawTransactionJson.Output(address, asset, amount) |]))
                .JsonValue.Request
            |> saveRawToFile
        | Some (RawTx_Sign args) ->
            let password, file = args.GetResult <@ RawTransactionSign_Arguments @>
            let raw = System.IO.File.ReadAllText file
            "wallet/rawtransaction/sign"
            |> getUri
            |> (new SignRawTransactionJson.Root(raw, password) )
                .JsonValue.Request
            |> saveRawToFile
        | Some (RawTx_Publish args) ->
            let file = args.GetResult <@ RawTransactionPublish_Arguments @>
            let raw = System.IO.File.ReadAllText file
            "wallet/rawtransaction/publish"
            |> getUri
            |> (new TxHexJson.Root(raw))
                .JsonValue.Request
            |> printResponse
        | Some (Wallet_Create args) ->
            let password = args.GetResult <@ Password_Arguments @>

            let words = (new NBitcoin.Mnemonic(NBitcoin.Wordlist.English, NBitcoin.WordCount.TwentyFour)).Words

            "wallet/import"
            |> getUri
            |> (new ImportSeedJson.Root(password, words)).JsonValue.Request
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
