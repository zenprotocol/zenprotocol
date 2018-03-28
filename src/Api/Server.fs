module Api.Server

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
                let address = Address.encode chain (Address.Contract contract.contractHash)
                new ActiveContractsJson.Root(Hash.toString contract.contractHash,address, contract.expiry |> int,contract.code))
            |> List.map (fun json -> json.JsonValue)
            |> List.toArray
            |> JsonValue.Array

        reply StatusCode.OK (JsonContent json)
    | Get ("/contract/hash", query) ->
        match Map.tryFind "address" query with
        | None ->
              TextContent (sprintf "address is missing")
              |> reply StatusCode.BadRequest
        | Some address ->
            match Address.decodeContract chain address with
            | FSharp.Core.Ok cHash ->
                TextContent (Hash.toString cHash)
                |> reply StatusCode.OK
            | FSharp.Core.Error _ ->
                TextContent (sprintf "invalid address %A" query)
                |> reply StatusCode.BadRequest

    | Get ("/wallet/balance", _) ->
        match Wallet.getBalance client with
        | Ok balances ->
            balances
            |> Map.toSeq
            |> Seq.map (fun ((asset, assetType), amount) -> new BalanceResponseJson.Root(Hash.toString asset, Hash.toString assetType, int64 amount))
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
            (new AccountExistsResponseJson.Root(result)).JsonValue
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            replyError error
    | Get ("/wallet/address", _) ->
        match Wallet.getAddress client with
        | Ok address ->
            (new AddressJson.Root(address)).JsonValue
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            replyError error
    | Post ("/wallet/import", Some body) ->
        match getImportSeed body with
        | Ok words ->
            match Wallet.importSeed client words with
            | Ok _ -> reply StatusCode.OK NoContent
            | Error error -> replyError error
        | Error error ->
            replyError error
    | Post ("/wallet/spend", Some body) ->
        match getSpend chain body with
        | Ok (pkHash, spend) ->
            Wallet.createTransaction client pkHash spend
            |> validateTx
        | Error error ->
            replyError error
    | Get ("/wallet/transactions", _) ->
        match Wallet.getTransactions client with
        | Ok txs ->
            let json =
                txs
                |> List.toArray
                |> Array.map (fun (txHash, amounts) ->
                    let deltas =
                        amounts
                        |> Map.toArray
                        |> Array.map (fun ((asset, assetType), amount) ->
                            new TransactionsResponseJson.Delta(Hash.toString asset, Hash.toString assetType, amount))
                    (new TransactionsResponseJson.Root(Hash.toString txHash, deltas)).JsonValue)
                |> JsonValue.Array
            (new TransactionsResponseJson.Root(json)).JsonValue
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            replyError error
    | Post ("/wallet/contract/activate", Some body) ->
        match getContractActivate body with
        | Error error -> replyError error
        | Ok (code,numberOfBlocks) ->
            match Wallet.activateContract client code numberOfBlocks with
            | Ok (tx, cHash) ->
                let address =
                    Address.Contract cHash
                    |> Address.encode chain
                Blockchain.validateTransaction client tx
                let json = new ContractActivateResponseJson.Root (address, Hash.toString cHash)
                reply StatusCode.OK (JsonContent json.JsonValue)
            | Error error ->
                replyError error
    | Post ("/wallet/contract/execute", Some body) ->
        match getContractExecute chain body with
        | Error error -> replyError error
        | Ok (cHash, command, data, spends) ->
            Wallet.executeContract client cHash command data spends
            |> validateTx
    | Post ("/wallet/resync", _) ->
        Wallet.resyncAccount client
        reply StatusCode.OK NoContent
    | Post ("/block/publish", Some body) ->
            match getPublishBlock body with
            | Error error -> replyError error
            | Ok block ->
                Blockchain.validateMinedBlock client block
                reply StatusCode.OK NoContent
    | _ ->
        reply StatusCode.NotFound NoContent

let create chain poller busName bind =
    let httpAgent = Http.Server.create poller bind

    Log.info "Api running on %s" bind

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
