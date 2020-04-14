module Cli.Request

open FSharp.Data
open Api.Types
open Util


module Request =

    let send getUri address asset amount password =
        "wallet/send"
        |> getUri
        |> SendRequestJson.Root(
            [| SendRequestJson.Output(address, asset, amount) |],
            password
            ).JsonValue.Request
        |> getResponse

    let address getUri =
        "wallet/address"
        |> getUri
        |> Http.Request
        |> getResponse

    let resync getUri =
        "wallet/resync"
        |> getUri
        |> Http.Request
        |> getResponse

    let import getUri words pass =
        "wallet/import"
        |> getUri
        |> ImportSeedJson.Root(pass, words).JsonValue.Request
        |> getResponse

    let activateContract getUri code numberOfBlocks pass =
        "wallet/contract/activate"
        |> getUri
        |> ContractActivateRequestJson.Root(
            code, numberOfBlocks, pass
            ).JsonValue.Request 
        |> getResponse

    let extend getUri address numberOfBlocks pass =
        "wallet/contract/extend"
        |> getUri
        |> ContractExtendRequestJson.Root(address, numberOfBlocks, pass).JsonValue.Request
        |> getResponse

    let execute getUri address command messageBody asset amount password =
        let messageBody = if checker messageBody then "" else messageBody

        let spend = if checker asset || amount = 0L then [||] else [| ContractExecuteRequestJson.Spend(asset, amount) |]

        "wallet/contract/execute"
        |> getUri
        |> (ContractExecuteRequestJson.Root (
                address, command, messageBody,
                ContractExecuteRequestJson.Options(true, ""),
                spend,
                password
           )).JsonValue.Request
        |> getResponse

    let publishBlock getUri (block : string) =
        "blockchain/publishblock"
        |> getUri
        |> PublishBlockJson.Root(block).JsonValue.Request
        |> getResponse

    let walletExists getUri =
        "wallet/exists"
        |> getUri
        |> Http.Request
        |> getResponse

    let private passwordJson getUri (pass : string) url =
        url
        |> getUri
        |> CheckPasswordJson.Root(pass).JsonValue.Request


    let checkPassword getUri pass =
        "wallet/checkpassword"
        |> passwordJson getUri pass
        |> getResponse

    let mnemonicPhrase getUri (pass : string) =
        "wallet/mnemonicphrase"
        |> passwordJson getUri pass
        |> getResponse

    let importZenPublicKey getUri (publicKey : string) =
        "wallet/importzenpublickey"
        |> getUri
        |> ImportZenPublicKey.Root(publicKey).JsonValue.Request
        |> getResponse

    let exportZenPublicKey getUri =
        "wallet/zenpublickey"
        |> getUri
        |> Http.Request
        |> getResponse

    let removeWallet getUri (pass : string) =
        "wallet/remove"
        |> passwordJson getUri pass
        |> getResponse

    let publicKey getUri path pass =
        "wallet/publickey"
        |> getUri
        |> GetPublicKeyJson.Root(path, pass).JsonValue.Request
        |> getResponse

    let createRawTxRequest getUri asset amount address : HttpResponse =
        "wallet/rawtransaction/create"
        |> getUri
        |> CreateRawTransactionJson.Root(
            [| CreateRawTransactionJson.Output(address, asset, amount) |]
            ).JsonValue.Request

    let signRawTxRequest getUri raw pass : HttpResponse =
        "wallet/rawtransaction/sign"
        |> getUri
        |> SignRawTransactionJson.Root(raw, pass).JsonValue.Request

    let publishRawTx getUri (raw : string) =
        "wallet/rawtransaction/publish"
        |> getUri
        |> TxHexJson.Root(raw).JsonValue.Request
        |> getResponse

    let walletKeys getUri pass =
        "wallet/keys"
        |> passwordJson getUri pass
        |> getResponse

    let signMessage getUri message path pass =
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
             tipBlockHash = (Consensus.Hash.fromString json.Tip |> Infrastructure.Result.get)
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
