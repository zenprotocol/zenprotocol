module Api.Server

open FSharp.Data
open Consensus
open Infrastructure
open Infrastructure.ServiceBus
open Infrastructure.Http
open Api.Types
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

let handleRequest client (request,reply) =    
    match request with
    | Get ("/wallet/balance", _) ->            
        let balances = Wallet.getBalance client

        let json = 
            balances
            |> Map.toSeq
            |> Seq.map (fun (key,value) -> new BalanceJson.Root(Hash.toString key, int64 value)) 
            |> Seq.map (fun balance -> balance.JsonValue)
            |> Seq.toArray
            |> JsonValue.Array
    
        reply StatusCode.OK (JsonContent json)
    | Get ("/wallet/address", _) ->
        let address = Wallet.getAddress client 
    
        let json = new AddressJson.Root(address)
        reply StatusCode.OK (JsonContent json.JsonValue)
        
    | Post ("/wallet/transaction/send", Some body) ->
        let transactionSend = TransactionSendJson.Parse (body)        
        
        match Hash.fromString transactionSend.Asset with
        | None -> 
            reply StatusCode.BadRequest (TextContent "asset not valid")
        | Some assetHash ->         
            
            // TODO: validate address and only send the wallet the pkHash
            
            match Wallet.createTransaction client transactionSend.To assetHash (uint64 transactionSend.Amount) with
            | Error error -> reply StatusCode.BadRequest (TextContent error)
            | Created tx ->
                Blockchain.validateTransaction client tx
                reply StatusCode.OK NoContent
    | Post ("/wallet/contract/activate", Some body) ->
        let activateContract = ContractActivationJson.Parse (body)        
        
        if String.length activateContract.Code = 0 then
            reply StatusCode.BadRequest (TextContent "code cannot be empty")
        else
            match Wallet.createContractActivationTransaction client activateContract.Code with
            | Error error -> reply StatusCode.BadRequest (TextContent error)
            | Created tx ->
                Blockchain.validateTransaction client tx
                reply StatusCode.OK NoContent
    | Post ("/wallet/contract/send", Some body) ->
        let contractMessageSend = ContractMessageSendJson.Parse (body)
        
        match Wallet.createSendMessageTranscation client contractMessageSend.To Hash.zero (uint64 contractMessageSend.Amount) with
        | Error error -> reply StatusCode.BadRequest (TextContent error)
        | Created tx ->
            Blockchain.validateTransaction client tx
            reply StatusCode.OK NoContent
    | _ ->
        reply StatusCode.NotFound NoContent    

let create poller busName bind = 
    let httpAgent = Http.Server.create poller bind
    
    Log.info "Api running on %s" bind
    
    let client = Client.create busName
    
    let observable = 
        Http.Server.observable httpAgent 
        |> Observable.map (fun request -> 
            fun (server:T) ->
                handleRequest client request
                server
        )
        
    {agent = httpAgent; observable = observable; client = client}
    
let observable server = server.observable              