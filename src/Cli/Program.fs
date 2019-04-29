open Argu
open System
open System.IO
open FSharp.Data
open Api.Types

type PaginationArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] Pagination_Arguments of skip:int * take:int
    interface IArgParserTemplate with
        member arg.Usage = ""

type SendArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] Send_Arguments of asset:string * amount:int64 * address:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type ActivateContractArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] ActivateContract_Arguments of file:string * numberOfBlocks:int
    interface IArgParserTemplate with
        member arg.Usage = ""

type ExtendContractArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] ExtendContract_Arguments of address:string * numberOfBlocks:int
    interface IArgParserTemplate with
        member arg.Usage = ""

type ExecuteContractArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] ExecuteContract_Arguments of address:string * command:string * messageBody:string * asset:string * amount:int64
    interface IArgParserTemplate with
        member arg.Usage = ""

type PublishBlockArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] PublishBlock_Arguments of block:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type PublicKeyArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] PublicKey_Arguments of path:string
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
    | [<MainCommand("COMMAND");ExactlyOnce>] RawTransactionSign_Arguments of file:string
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
    | [<CliPrefix(CliPrefix.None)>] Import of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] Send of ParseResults<SendArgs>
    | [<CliPrefix(CliPrefix.None)>] Activate of ParseResults<ActivateContractArgs>
    | [<CliPrefix(CliPrefix.None)>] Extend of ParseResults<ExtendContractArgs>
    | [<CliPrefix(CliPrefix.None)>] Execute of ParseResults<ExecuteContractArgs>
    | [<CliPrefix(CliPrefix.None)>] Active of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] PublishBlock of ParseResults<PublishBlockArgs>
    | [<CliPrefix(CliPrefix.None)>] AccountExists of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] CheckPassword of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] MnemonicPhrase of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] ExportZenPublicKey of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] ImportZenPublicKey of ParseResults<ImportPublicKeyArgs>
    | [<CliPrefix(CliPrefix.None)>] RemoveWallet of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] PublicKey of ParseResults<PublicKeyArgs>
    | [<CliPrefix(CliPrefix.None)>] RawTx_Create of ParseResults<RawTransactionCreateArgs>
    | [<CliPrefix(CliPrefix.None)>] RawTx_Sign of ParseResults<RawTransactionSignArgs>
    | [<CliPrefix(CliPrefix.None)>] RawTx_Publish of ParseResults<RawTransactionPublishArgs>
    | [<CliPrefix(CliPrefix.None)>] Wallet_Create of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] Blockchain_Info of ParseResults<NoArgs>

    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Port _ -> "port of zen-node API"
            | Test _ -> "use testnet port"
            | Balance _ -> "get wallet balance"
            | History _ -> "list wallet transactions"
            | Address _ -> "get wallet address"
            | Resync _ -> "resync wallet"
            | Import _ -> "import wallet seed from mnemonic sentence"
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
            | Wallet_Create _ -> "create a wallet from a newly generated mnemonic phrase"
            | Blockchain_Info _ -> "get blockchain info"
            | _ -> ""


let mutable port = 11567us
//let mutable port = 31567us

let getUri = sprintf "http://127.0.0.1:%d/%s" port
    
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

let printResponse = getResponse >> printfn "%s"

// read from console, hiding input text
let readMasked(): string =
    let rec read (acc: list<char>): string =
        let keyInfo = Console.ReadKey(true)
        if keyInfo.Key = ConsoleKey.Enter then
            List.rev acc
            |> Array.ofList
            |> System.String
        elif keyInfo.Key = ConsoleKey.Backspace then
            match acc with
            | _::tl | tl -> read tl
        elif Char.IsControl keyInfo.KeyChar then
            read acc
        else
            read (keyInfo.KeyChar::acc)
    read []

// prompts the user to input a password, and returns the supplied password
let readPassword(): string =
    printfn "Enter password:"
    readMasked()

// creates a wallet and password
let rec createWallet(): unit =
    let password1 = readPassword()
    printfn "Enter password again:"
    let password2 = readMasked()
    if password1 <> password2 then
        printfn "Passwords do not match, please try again"; createWallet()
    else
        let words = NBitcoin.Mnemonic(NBitcoin.Wordlist.English, NBitcoin.WordCount.TwentyFour).Words
        getUri "wallet/import"
        |> ImportSeedJson.Root(password1, words).JsonValue.Request
        |> printResponse

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>(programName = "zen-ctl", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    List.iter (fun arg ->
        match arg with
        | Port p -> port <- p
        | Test -> port <- 31567us
#if DEBUG
        | Local idx -> port <- 20000us + idx
#endif
        | _ -> ()
        ) (results.GetAllResults())

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
            let asset, amount, address= args.GetResult <@ Send_Arguments @>
            let password = readPassword()
            "wallet/send"
            |> getUri
            |> SendRequestJson.Root(
                [| SendRequestJson.Output(address, asset, amount) |],
                password
                ).JsonValue.Request
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
        | Some (Import _) ->
            printfn "Enter seed words, separated by spaces"
            let words = Console.ReadLine().Split [|' '|]
            let pass = readPassword()
            "wallet/import"
            |> getUri
            |> ImportSeedJson.Root(pass, words).JsonValue.Request
            |> printResponse

        | Some (Activate args) ->
            let file, numberOfBlocks = args.GetResult <@ ActivateContract_Arguments @>
            let password = readPassword()
            match File.Exists file with
            | false ->
                printfn "File not found: %s" file
            | true ->
                let code = File.ReadAllText file
                let result =
                    "wallet/contract/activate"
                    |> getUri
                    |> ContractActivateRequestJson.Root(
                        code, numberOfBlocks, password
                        ).JsonValue.Request
                    |> getResponse
                    |> ContractActivateResponseJson.Parse

                printfn "Contract activated.\nAddress: %s\nContract Id: %s" result.Address result.ContractId
        | Some (Extend args) ->
            let address, numberOfBlocks = args.GetResult <@ ExtendContract_Arguments @>
            let password = readPassword()
            "wallet/contract/extend"
            |> getUri
            |> ContractExtendRequestJson.Root(address, numberOfBlocks, password)
               .JsonValue.Request
            |> printResponse
        | Some (Execute args) ->
            let address, command, messageBody, asset, amount =
                args.GetResult <@ ExecuteContract_Arguments @>

            let password = readPassword()

            let messageBody = if messageBody = "None" || messageBody = "none" || messageBody = "null" then "" else messageBody

            "wallet/contract/execute"
            |> getUri
            |> ContractExecuteRequestJson.Root(
                    address, command, messageBody,
                    ContractExecuteRequestJson.Options(true, ""),
                    [| ContractExecuteRequestJson.Spend(asset, amount) |],
                    password
               ).JsonValue.Request
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
            |> PublishBlockJson.Root(block).JsonValue.Request
            |> printResponse
        | Some (AccountExists _) ->
            "wallet/exists"
            |> getUri
            |> Http.Request
            |> printResponse
        | Some (CheckPassword _) ->
            let password = readPassword()
            "wallet/checkpassword"
            |> getUri
            |> CheckPasswordJson.Root(password).JsonValue.Request
            |> printResponse
        | Some (MnemonicPhrase _) ->
            let password = readPassword()
            "wallet/mnemonicphrase"
            |> getUri
            |> CheckPasswordJson.Root(password).JsonValue.Request
            |> printResponse
        | Some (ImportZenPublicKey args) ->
            let publicKey = args.GetResult <@ ImportPublicKey_Arguments @>
            "wallet/importzenpublickey"
            |> getUri
            |> ImportZenPublicKey.Root(publicKey).JsonValue.Request
            |> printResponse
        | Some (ExportZenPublicKey _) ->
            "wallet/zenpublickey"
            |> getUri
            |> Http.Request
            |> printResponse
        | Some (RemoveWallet _) ->
            let password = readPassword()
            "wallet/remove"
            |> getUri
            |> CheckPasswordJson.Root(password).JsonValue.Request
            |> printResponse
        | Some (PublicKey args) ->
            let path = args.GetResult <@ PublicKey_Arguments @>
            let password = readPassword()
            "wallet/publickey"
            |> getUri
            |> GetPublicKeyJson.Root(path, password).JsonValue.Request
            |> printResponse
        | Some (RawTx_Create args) ->
            let asset, amount, address = args.GetResult <@ RawTransactionCreate_Arguments @>
            "wallet/rawtransaction/create"
            |> getUri
            |> CreateRawTransactionJson.Root(
                [| CreateRawTransactionJson.Output(address, asset, amount) |]
                ).JsonValue.Request
            |> saveRawToFile
        | Some (RawTx_Sign args) ->
            let file = args.GetResult <@ RawTransactionSign_Arguments @>
            let raw = System.IO.File.ReadAllText file
            let password = readPassword()
            "wallet/rawtransaction/sign"
            |> getUri
            |> SignRawTransactionJson.Root(raw, password).JsonValue.Request
            |> saveRawToFile
        | Some (RawTx_Publish args) ->
            let file = args.GetResult <@ RawTransactionPublish_Arguments @>
            let raw = System.IO.File.ReadAllText file
            "wallet/rawtransaction/publish"
            |> getUri
            |> TxHexJson.Root(raw).JsonValue.Request
            |> printResponse
        | Some (Wallet_Create _) -> createWallet()
        | Some (Blockchain_Info _) ->
            "blockchain/info"
            |> getUri
            |> BlockChainInfoJson.Load
            |> fun json -> 
                printfn "chain: %s\nblocks: %d\nheaders: %d\ndifficulty: %A\nmedianTime: %i\ninitialBlockDownload: %A\ntip: %A"
                    json.Chain
                    json.Blocks
                    json.Headers
                    json.Difficulty
                    json.MedianTime
                    json.InitialBlockDownload
                    json.Tip
        | _ -> ()
    with
    | :? Net.WebException as ex ->
        let message = ex.Message
        if ex.Response <> null then
            use reader = new StreamReader(ex.Response.GetResponseStream())
            let body = reader.ReadToEnd()
            if body = message then message else sprintf "%s %s" message (reader.ReadToEnd())
        else
            message
        |> exit
    | :? AggregateException as ex ->
        exit (ex.Flatten().InnerException.Message)
    | ex ->
        exit ex.Message

    0
