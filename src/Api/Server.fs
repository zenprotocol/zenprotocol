module Api.Server

open Infrastructure
open Infrastructure.Http
open FSharp.Data
open Api.Types

type T = 
    {
        agent: Server.T
        observable: System.IObservable<T->T> 
    }
    interface System.IDisposable with
        member x.Dispose() =
            (x.agent :> System.IDisposable).Dispose()

let handleRequest (request,reply) =    
    match request with
    | Get ("/wallet/balance", _) ->            
        let balance = new BalanceJson.Root ("0", 2)
        let balances = JsonValue.Array [|balance.JsonValue|]
                    
        reply StatusCode.OK (JsonContent balances)
    | Get ("/wallet/address", _) ->
        let value = JsonValue.Parse ("""{"address":"abdasda"}""")
        reply StatusCode.OK (JsonContent value)
        
    | Post ("/wallet/transaction/send", Some body) ->
        let transactionSend = TransactionSendJson.Parse (body)
        
        // TODO: validate the data is correct
        
        reply StatusCode.OK NoContent
                         
    | _ ->
        reply StatusCode.NotFound NoContent    

let create poller bind = 
    let httpAgent = Http.Server.create poller bind
    
    Log.info "Api running on %s" bind
    
    let observable = 
        Http.Server.observable httpAgent 
        |> Observable.map (fun request -> 
            fun (server:T) ->
                handleRequest request
                server
        )
        
    {agent = httpAgent; observable = observable}
    
let observable server = server.observable              