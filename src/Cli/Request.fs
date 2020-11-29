module Cli.Request

open FSharp.Data
open Api.Types
open Util
open Consensus


module Request =

    let send
        (getUri: string -> string)
        (address:string)
        (asset:string)
        (amount:int64)
        (password:string) =
            "wallet/send"
            |> getUri
            |> SendRequestJson.Root(
                [| SendRequestJson.Output(address, asset, string amount) |],
                password
                ).JsonValue.Request
            |> getResponse

    let address
        (getUri: string -> string) =
            "wallet/address"
            |> getUri
            |> Http.Request
            |> getResponse

    let resync
        (getUri: string -> string)
        =
            "wallet/resync"
            |> getUri
            |> Http.Request
            |> getResponse

    let import
        (getUri: string -> string)
        (words:string[])
        (pass:string) =
            "wallet/import"
            |> getUri
            |> ImportSeedJson.Root(pass, words).JsonValue.Request
            |> getResponse

    let activateContract
        (getUri: string -> string)
        (code:string)
        (numberOfBlocks:int)
        (rlimit:int)
        (pass:string) =
            "wallet/contract/activate"
            |> getUri
            |> ContractActivateRequestJson.Root(
                code, numberOfBlocks, rlimit, pass
                ).JsonValue.Request 
            |> getResponse

    let extend
        (getUri: string -> string)
        (address:string)
        (numberOfBlocks:int)
        (pass:string) =
            "wallet/contract/extend"
            |> getUri
            |> ContractExtendRequestJson.Root(address, numberOfBlocks, pass).JsonValue.Request
            |> getResponse

    let execute
        (getUri: string -> string)
        (address:string)
        (command:string)
        (messageBody:string)
        (asset:string)
        (amount:int64)
        (password:string) =
            let messageBody = if checker messageBody then "" else messageBody

            let spend = if checker asset || amount = 0L then [||] else [| ContractExecuteRequestJson.Spend(asset, string amount) |]

            "wallet/contract/execute"
            |> getUri
            |> (ContractExecuteRequestJson.Root (
                    address, command, messageBody,
                    ContractExecuteRequestJson.Options(true, ""),
                    spend,
                    password
               )).JsonValue.Request
            |> getResponse

    let publishBlock
        (getUri: string -> string)
        (block : string) =
            "blockchain/publishblock"
            |> getUri
            |> PublishBlockJson.Root(block).JsonValue.Request
            |> getResponse

    let walletExists
        (getUri: string -> string)
        =
            "wallet/exists"
            |> getUri
            |> Http.Request
            |> getResponse

    let private passwordJson
        (getUri: string -> string)
        (pass : string)
        (url:string) =
            url
            |> getUri
            |> CheckPasswordJson.Root(pass).JsonValue.Request


    let checkPassword
        (getUri: string -> string)
        (pass: string)
        =
            "wallet/checkpassword"
            |> passwordJson getUri pass
            |> getResponse

    let mnemonicPhrase
        (getUri: string -> string)
        (pass : string) =
            "wallet/mnemonicphrase"
            |> passwordJson getUri pass
            |> getResponse

    let importZenPublicKey
        (getUri: string -> string)
        (publicKey : string) =
            "wallet/importzenpublickey"
            |> getUri
            |> ImportZenPublicKey.Root(publicKey).JsonValue.Request
            |> getResponse

    let exportZenPublicKey
        (getUri: string -> string)
        =
            "wallet/zenpublickey"
            |> getUri
            |> Http.Request
            |> getResponse

    let removeWallet
        (getUri: string -> string)
        (pass : string) =
            "wallet/remove"
            |> passwordJson getUri pass
            |> getResponse

    let publicKey
        (getUri: string -> string)
        (path:string)
        (pass:string) =
            "wallet/publickey"
            |> getUri
            |> GetPublicKeyJson.Root(path, pass).JsonValue.Request
            |> getResponse

    let createRawTxRequest
        (getUri: string -> string)
        (asset:string)
        (amount:int64)
        (address:string)
        : HttpResponse =
            "wallet/rawtransaction/create"
            |> getUri
            |> CreateRawTransactionJson.Root(
                [| CreateRawTransactionJson.Output(address, asset, string amount) |]
                ).JsonValue.Request

    let signRawTxRequest
        (getUri: string -> string)
        (raw:string)
        (pass: string)
        : HttpResponse =
            "wallet/rawtransaction/sign"
            |> getUri
            |> SignRawTransactionJson.Root(raw, pass).JsonValue.Request

    let publishRawTx
        (getUri: string -> string)
        (raw : string) =
            "wallet/rawtransaction/publish"
            |> getUri
            |> TxHexJson.Root(raw).JsonValue.Request
            |> getResponse

    let walletKeys
        (getUri: string -> string)
        (pass:string) =
            "wallet/keys"
            |> passwordJson getUri pass
            |> getResponse

    let signMessage
        (getUri: string -> string)
        (message:string)
        (path:string)
        (pass: string) =
            "wallet/sign"
            |> getUri
            |> SignJson.Root(message, path, pass).JsonValue.Request
            |> getResponse


module Load =

    let blockchainInfo (getUri : string -> string) : Messaging.Services.Blockchain.BlockchainInfo =
        "blockchain/info"
        |> getUri
        |> BlockChainInfoJson.Load
        |> fun (json : BlockChainInfoJson.Root) ->
            {
             chain = json.Chain
             blocks = (uint32 json.Blocks)
             headers = (uint32 json.Headers)
             difficulty = (float json.Difficulty)
             medianTime = (uint64 json.MedianTime)
             initialBlockDownload = json.InitialBlockDownload
             tipBlockHash = (Hash.fromString json.Tip |> Option.get)
            }
            
    let activeContracts (getUri : string -> string) =
        "contract/active"
        |> getUri
        |> ActiveContractsResponseJson.Load
        |> Array.map (fun json -> json.Address, json.ContractId, json.Expire, json.Code)
    
    let walletTransaction (getUri : string -> string) skip take =
        sprintf "wallet/transactions?skip=%d&take=%d" skip take
        |> getUri
        |> TransactionsResponseJson.Load
        |> Array.map (fun json -> json.TxHash, json.Asset, json.Amount, json.Confirmations)

    let balance (getUri: string -> string) =
        "wallet/balance"
        |> getUri
        |> BalanceResponseJson.Load
        |> Array.map (fun json -> json.Asset, json.Balance)

    let getCGP (getUri: string -> string) =
        "blockchain/cgp"
        |> getUri
        |> WinnerTally.Load
        |> fun json -> (json.Allocation, PayoutResultJson.Root(json.Payout.JsonValue))
