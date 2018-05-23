module Api.Server

open System.Security.Cryptography.X509Certificates
open System.Windows.Input
open FSharp.Data
open Consensus
open Infrastructure
open Infrastructure.ServiceBus
open Infrastructure.Http
open Api.Types
open Parsing
open Messaging.Services
open Messaging.Services.Wallet
open Result
open Zen.Crypto
open Consensus.Crypto
open Logary.Message

type T =
    {
        client: Client.T
        agent: Server.T
        observable: System.IObservable<T->T>
    }
    interface System.IDisposable with
        member x.Dispose() =
            (x.client :> System.IDisposable).Dispose()
            (x.agent :> System.IDisposable).Dispose()

let handleRequest chain client (request,reply) =
    let replyError error =
        reply StatusCode.BadRequest (TextContent error)

    let validateTx result =
        match result with
        | Error error ->
            replyError error
        | Ok tx ->
            Blockchain.validateTransaction client tx
            reply StatusCode.OK NoContent

    match request with
    | Get ("/network/connections/count", _) ->
        let count = Network.getConnectionCount client

        reply StatusCode.OK (JsonContent (JsonValue.Number (count |> decimal)))

    | Get ("/blockchain/headers", _) ->
        let headers = Blockchain.getHeaders client

        let json =
            headers
            |> List.map (fun header ->
                let hash = Block.hash header
                new HeadersResponseJson.Root(
                                                Hash.toString hash, header.timestamp |> int64,
                                                Timestamp.toString header.timestamp,
                                                header.blockNumber |> int,
                                                "0x" + header.difficulty.ToString("x"),
                                                Difficulty.uncompress header.difficulty |> Hash.toString
                                                ))
            |> List.map (fun json -> json.JsonValue)
            |> List.toArray
            |> JsonValue.Array

        reply StatusCode.OK (JsonContent json)

    | Get ("/blockchain/info", _) ->
        let info = Blockchain.getBlockChainInfo client

        let json = (
            new BlockChainInfoJson.Root(
                info.chain,
                info.blocks|> int,
                info.headers |> int,
                info.difficulty |> decimal,
                info.medianTime |> int64)).JsonValue

        reply StatusCode.OK (JsonContent json)

    | Get ("/contract/active", _) ->
        let activeContracts = Blockchain.getActiveContracts client
        let json =
            activeContracts
            |> List.map (fun contract ->
                let address = Address.encode chain (Address.Contract contract.contractId)
                new ActiveContractsResponseJson.Root(ContractId.toString contract.contractId, address, contract.expiry |> int,contract.code))
            |> List.map (fun json -> json.JsonValue)
            |> List.toArray
            |> JsonValue.Array

        reply StatusCode.OK (JsonContent json)
    | Get ("/contract/contractId", query) ->
        match Map.tryFind "address" query with
        | None ->
              TextContent (sprintf "address is missing")
              |> reply StatusCode.BadRequest
        | Some address ->
            match Address.decodeContract chain address with
            | FSharp.Core.Ok contractId ->
                TextContent (ContractId.toString contractId)
                |> reply StatusCode.OK
            | FSharp.Core.Error _ ->
                TextContent (sprintf "invalid address %A" query)
                |> reply StatusCode.BadRequest
    | Post ("/wallet/publickey", Some body) ->
        match parseGetPublicKeyJson body with
        | Error error -> replyError error
        | Ok (path, password) ->
            match Wallet.getPublicKey client path password with
            | Ok key ->
                PublicKey.toString key
                |> TextContent
                |> reply StatusCode.OK
            | Error error ->
                TextContent error
                |> reply StatusCode.BadRequest
    | Get ("/wallet/balance", _) ->
        match Wallet.getBalance client with
        | Ok balances ->
            balances
            |> Map.toSeq
            |> Seq.map (fun (asset, amount) -> new BalanceResponseJson.Root(Asset.toString asset, int64 amount))
            |> Seq.map (fun json -> json.JsonValue)
            |> Seq.toArray
            |> JsonValue.Array
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            replyError error
    | Get ("/wallet/exists", _) ->
        match Wallet.accountExists client with
        | Ok result ->
            result
            |> JsonValue.Boolean
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            replyError error
    | Post ("/wallet/checkpassword", Some body) ->
        match parseCheckPasswordJson body with
        | Ok password ->
            match Wallet.checkPassword client password with
            | Ok result ->
                result
                |> JsonValue.Boolean
                |> JsonContent
                |> reply StatusCode.OK
            | Error error ->
                replyError error
        | Error error ->
            replyError error
    | Get ("/wallet/address", _) ->
        match Wallet.getAddress client with
        | Ok address ->
            address
            |> JsonValue.String
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            replyError error
    | Post ("/wallet/import", Some body) ->
        match parseImportSeedJson body with
        | Ok (words, key) ->
            match Wallet.importSeed client words key with
            | Ok _ -> reply StatusCode.OK NoContent
            | Error error -> replyError error
        | Error error ->
            replyError error
    | Post ("/wallet/mnemonicphrase", Some body) ->
        // TODO: should be a get with Authorization header
        match parseCheckPasswordJson body with
        | Ok password ->
            match Wallet.getMnemonicPhrase client password with
            | Ok mnemonicPhrase ->
                mnemonicPhrase
                |> JsonValue.String
                |> JsonContent
                |> reply StatusCode.OK
            | Error error ->
                replyError error
        | Error error ->
            replyError error
    | Post ("/wallet/send", Some body) ->
        match parseSendJson chain body with
        | Ok (pkHash, spend, password) ->
            Wallet.createTransaction client pkHash spend password
            |> validateTx
        | Error error ->
            replyError error
    | Post ("/wallet/transactions", Some body) ->
        match parseTransactionsRequestJson body with
        | Ok (skip, take) ->
            match Wallet.getTransactions client skip take with
            | Ok txs ->
                let json =
                    txs
                    |> List.toArray
                    |> Array.map (fun (txHash, amounts, blockNumer) ->
                        let deltas =
                            amounts
                            |> Map.toArray
                            |> Array.map (fun (asset, amount) ->
                                new TransactionsResponseJson.Delta(Asset.toString asset, amount))
                        (new TransactionsResponseJson.Root(Hash.toString txHash, deltas, int blockNumer)).JsonValue)
                    |> JsonValue.Array
                (new TransactionsResponseJson.Root(json)).JsonValue
                |> JsonContent
                |> reply StatusCode.OK
            | Error error ->
                replyError error
        | Error error ->
            replyError error
    | Post ("/wallet/contract/activate", Some body) ->
        match parseContractActivateJson body with
        | Error error -> replyError error
        | Ok (code, numberOfBlocks, password) ->
            match Wallet.activateContract client code numberOfBlocks password with
            | Ok (tx, contractId) ->
                let address =
                    Address.Contract contractId
                    |> Address.encode chain
                Blockchain.validateTransaction client tx
                let json = new ContractActivateResponseJson.Root (address, ContractId.toString contractId)
                reply StatusCode.OK (JsonContent json.JsonValue)
            | Error error ->
                replyError error
    | Post ("/wallet/contract/extend", Some body) ->
        match parseContractExtendJson chain body with
        | Error error -> replyError error
        | Ok (contractId, numberOfBlocks, password) ->
            match Wallet.extendContract client contractId numberOfBlocks password with
            | Ok tx ->
                Blockchain.validateTransaction client tx
                reply StatusCode.OK NoContent
            | Error error ->
                replyError error
    | Post ("/wallet/contract/execute", Some body) ->
        match parseContractExecuteJson chain body with
        | Error error -> replyError error
        | Ok (contractId, command, data, returnAddress, sign, spends, password) ->
            Wallet.executeContract client contractId command data returnAddress sign spends password
            |> validateTx
    | Get ("/wallet/resync", _) ->
        Wallet.resyncAccount client
        reply StatusCode.OK NoContent
    | Post ("/blockchain/publishblock", Some body) ->
        match parsePublishBlockJson body with
        | Error error -> replyError error
        | Ok block ->
            Blockchain.validateMinedBlock client block
            reply StatusCode.OK NoContent
    | Get ("/blockchain/blocktemplate", query) ->
        let pkHash =
            match Map.tryFind "address" query with
            | None -> Wallet.getAddressPKHash client
            | Some address -> Address.decodePK chain address

        match pkHash with
        | FSharp.Core.Ok pkHash ->
            let block =
                Blockchain.getBlockTemplate client pkHash
                |> Block.toHex

            JsonValue.String block
            |> JsonContent
            |> reply StatusCode.OK
        | FSharp.Core.Error _ ->
            TextContent (sprintf "invalid address %A" query)
            |> reply StatusCode.BadRequest
    | _ ->
        reply StatusCode.NotFound NoContent

let create chain poller busName bind =
    let httpAgent = Http.Server.create poller bind

    eventX "Api running on {bind}"
    >> setField "bind" (sprintf "http://%s" bind)
    |> Log.info

    let client = Client.create busName

    let observable =
        Http.Server.observable httpAgent
        |> Observable.map (fun request ->
            fun (server:T) ->
                handleRequest chain client request
                server
        )

    {agent = httpAgent; observable = observable; client = client}

let observable server = server.observable
