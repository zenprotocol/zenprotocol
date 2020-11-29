open Argu
open System
open System.IO
open FSharp.Data
open Api.Types
open Cli
open Cli.Util
open Cli.Request.Request
open Cli.Request.Load

type PaginationArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] Pagination_Arguments of skip:int * take:int
    interface IArgParserTemplate with
        member arg.Usage = ""

type SendArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] Send_Arguments of asset:string * amount:int64 * address:string
    interface IArgParserTemplate with
        member arg.Usage = ""

type ActivateContractArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] ActivateContract_Arguments of file:string * numberOfBlocks:int * rlimit:int
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
type SignArgs =
    | [<MainCommand("COMMAND");ExactlyOnce>] Sign_Arguments of message:string * path:string
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
    | [<CliPrefix(CliPrefix.None)>] WalletKeys of ParseResults<NoArgs>
    | [<CliPrefix(CliPrefix.None)>] SignMessage of ParseResults<SignArgs>

    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
#if DEBUG
            | Local _ -> "use local"
#endif
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
            | WalletKeys _ -> "get wallet keys"
            | SignMessage _ -> "get wallet keys"

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

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>(programName = "zen-cli", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    let mutable getUri : (string -> string) = getUri' "11567"

    List.iter (function
        | Port p -> getUri <- getUri' (string p)
        | Test -> getUri <- getUri' "31567"
#if DEBUG
        | Local idx -> getUri <- getURL (int idx)
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
            Request.Request.send getUri address asset amount password
            |> printResponse
        | Some (Balance _) ->
            let balance =
                balance getUri

            printfn "Asset\t\t| Balance"
            printfn "============================"

            Array.iter (fun (asset, assetBalance) ->
                printfn " %s\t| %d" asset assetBalance) balance
        | Some (History args) ->
            let skip, take = args.GetResult <@ Pagination_Arguments @>
            let transactions =
                walletTransaction getUri skip take

            printfn "TxHash\t| Asset\t| Amount\t|Confirmations"
            printfn "==================================================="

            Array.iter (fun (txHash, asset, amount, confirmations) ->
                printfn "%s\t%s\t%s\t%d" txHash asset amount confirmations
            ) transactions
        | Some (Address _) ->
            let result =
                address getUri
                |> JsonValue.String
            printfn "%s" (result.AsString())
        | Some (Resync _) ->
            resync getUri
            |> printResponse
        | Some (Import _) ->
            printfn "Enter seed words, separated by spaces"
            let words = Console.ReadLine().Split [|' '|]
            let pass = readPassword()
            import getUri words pass
            |> printResponse
        | Some (Activate args) ->
            let file, numberOfBlocks, rlimit = args.GetResult <@ ActivateContract_Arguments @>
            let password = readPassword()
            match File.Exists file with
            | false ->
                printfn "File not found: %s" file
            | true ->
                let code = File.ReadAllText file
                let result =
                    activateContract getUri code numberOfBlocks rlimit password
                    |> ContractActivateOrExtendResponseJson.Parse

                printfn "Contract activated.\nAddress: %s\nContract Id: %s" result.Address result.ContractId
        | Some (Extend args) ->
            let address, numberOfBlocks = args.GetResult <@ ExtendContract_Arguments @>
            let password = readPassword()
            extend getUri address numberOfBlocks password
            |> printResponse
        | Some (Execute args) ->
            let address, command, messageBody, asset, amount =
                args.GetResult <@ ExecuteContract_Arguments @>

            let password = readPassword()


            execute getUri address command messageBody asset amount password
            |> printResponse
        | Some (Active _) ->
            let activeContracts =
                 activeContracts getUri

            printfn "Address\t\t| Contract Id\t\t | Expire"
            printfn "========================================="

            Array.iter (fun (address,contractId, expire, _) ->
                printfn " %s %s\t| %d" address contractId expire) activeContracts
        | Some (PublishBlock args) ->
            let block = args.GetResult <@ PublishBlock_Arguments @>
            publishBlock getUri block
            |> printResponse
        | Some (AccountExists _) ->
            walletExists getUri
            |> printResponse
        | Some (CheckPassword _) ->
            let password = readPassword()
            checkPassword getUri password
            |> printResponse
        | Some (MnemonicPhrase _) ->
            let password = readPassword()
            mnemonicPhrase getUri password
            |> printResponse
        | Some (ImportZenPublicKey args) ->
            let publicKey = args.GetResult <@ ImportPublicKey_Arguments @>
            importZenPublicKey getUri publicKey
            |> printResponse
        | Some (ExportZenPublicKey _) ->
            exportZenPublicKey getUri
            |> printResponse
        | Some (RemoveWallet _) ->
            let password = readPassword()
            removeWallet getUri password
            |> printResponse
        | Some (PublicKey args) ->
            let path = args.GetResult <@ PublicKey_Arguments @>
            let password = readPassword()
            publicKey getUri path password
            |> printResponse
        | Some (RawTx_Create args) ->
            let asset, amount, address = args.GetResult <@ RawTransactionCreate_Arguments @>
            createRawTxRequest getUri asset amount address
            |> saveRawToFile
        | Some (RawTx_Sign args) ->
            let file = args.GetResult <@ RawTransactionSign_Arguments @>
            let raw = System.IO.File.ReadAllText file
            let password = readPassword()
            signRawTxRequest getUri raw password
            |> saveRawToFile
        | Some (RawTx_Publish args) ->
            let file = args.GetResult <@ RawTransactionPublish_Arguments @>
            let raw = System.IO.File.ReadAllText file
            publishRawTx getUri raw
            |> printResponse
        | Some (Wallet_Create _) ->
            // creates a wallet and password
            let rec createWallet(): unit =
                let password1 = readPassword()
                printfn "Enter password again:"
                let password2 = readMasked()
                if password1 <> password2 then
                    printfn "Passwords do not match, please try again"; createWallet()
                else
                    let words = NBitcoin.Mnemonic(NBitcoin.Wordlist.English, NBitcoin.WordCount.TwentyFour).Words
                    import getUri words password1
                    |> printResponse
            createWallet()
        | Some (Blockchain_Info _) ->
            blockchainInfo getUri
            |> fun json ->
                printfn "chain: %s\nblocks: %d\nheaders: %d\ndifficulty: %A\nmedianTime: %i\ninitialBlockDownload: %A\ntip: %A"
                    json.chain
                    json.blocks
                    json.headers
                    json.difficulty
                    json.medianTime
                    json.initialBlockDownload
                    json.tipBlockHash
        | Some (WalletKeys _) ->
            walletKeys getUri (readPassword())
            |> printResponse
        | Some (SignMessage args) ->
            let message, path = args.GetResult <@ Sign_Arguments @>
            signMessage getUri message path (readPassword())
            |> printResponse
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
