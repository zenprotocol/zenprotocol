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
        | TransactionResult.Error error ->
            replyError error
        | TransactionResult.Ok tx ->
            Blockchain.validateTransaction client tx
            reply StatusCode.OK NoContent

    match request with
    | Get ("/contract/active", _) ->
        let activeContracts = Blockchain.getActiveContracts client
        let json =
            activeContracts
            |> List.map (fun contract ->
                let address = Address.encode chain (Address.Contract contract.contractHash)
                new ActiveContractsJson.Root(Hash.toString contract.contractHash,address, contract.expiry |> int,contract.code))
            |> List.map (fun balance -> balance.JsonValue)
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
        let balances = Wallet.getBalance client

        let json =
            balances
            |> Map.toSeq
            |> Seq.map (fun ((asset, assetType), amount) -> new BalanceResponseJson.Root(Hash.toString asset, Hash.toString assetType, int64 amount))
            |> Seq.map (fun balance -> balance.JsonValue)
            |> Seq.toArray
            |> JsonValue.Array

        reply StatusCode.OK (JsonContent json)
    | Get ("/wallet/address", _) ->
        let address = Wallet.getAddress client
        let json = new AddressJson.Root(address)
        reply StatusCode.OK (JsonContent json.JsonValue)
    | Post ("/wallet/spend", Some body) ->
        match getSpend chain body with
        | Result.Error error -> replyError error
        | Result.Ok (pkHash, spend) ->
            Wallet.createTransaction client pkHash spend
            |> validateTx
    | Post ("/wallet/contract/activate", Some body) ->
        match getContractActivate body with
        | Result.Error error -> replyError error
        | Result.Ok (code,numberOfBlocks) ->
            match Wallet.activateContract client code numberOfBlocks with
            | ActivateContractTransactionResult.Error error ->
                replyError error
            | ActivateContractTransactionResult.Ok (tx, cHash) ->
                let address =
                    Address.Contract cHash
                    |> Address.encode chain
                Blockchain.validateTransaction client tx
                let json = new ContractActivateResponseJson.Root (address, Hash.toString cHash)
                reply StatusCode.OK (JsonContent json.JsonValue)
    | Post ("/wallet/contract/execute", Some body) ->
        match getContractExecute chain body with
        | Result.Error error -> replyError error
        | Result.Ok (cHash, command, data, spends) ->
            Wallet.executeContract client cHash command data spends
            |> validateTx
    | Post ("/block/publish", Some body) ->
            match getPublishBlock body with
            | Result.Error error -> replyError error
            | Result.Ok block ->
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