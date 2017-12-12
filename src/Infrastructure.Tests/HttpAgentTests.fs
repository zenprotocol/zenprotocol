module Infrastructure.Tests.HttpAgentTests

open NUnit.Framework
open FsUnit
open FsNetMQ
open Infrastructure
open Infrastructure.Http
open FSharp.Control.Reactive
open FSharp.Data
open System.Net

module Actor = FsNetMQ.Actor 

let runServer _ =     
    Actor.create (fun shim ->
        let threadId = System.Threading.Thread.CurrentThread.ManagedThreadId
        use poller = Poller.create ()
        use observer = Poller.registerEndMessage poller shim
        
        use httpAgent = Server.create poller "127.0.0.1:45123"
        
        use observer = 
            Server.observable httpAgent |>
            Observable.subscribeWithError (fun (request,reply) -> 
                System.Threading.Thread.CurrentThread.ManagedThreadId |> should equal threadId
            
                match request with             
                | Get ("/hello", "") ->                                                            
                    reply StatusCode.OK (TextContent "get")
                    
                | Get ("/json", "") ->                                    
                    reply StatusCode.OK (JsonContent (JsonValue.Parse """{"balance":100}"""))                                                                                                  
                                                           
                | Post (path, body) ->                
                    path |> should equal "/hello"
                    body |> should equal (Some "[0,1,2]")
                                                            
                    reply StatusCode.OK (TextContent "post")
                | _ -> 
                    reply StatusCode.NotFound NoContent                                                                     
            ) (fun error -> 
                printfn "error %A" error
                raise error    
            )
        
        Actor.signal shim
        Poller.run poller        
    )

[<Test>]
let ``sending get request``() =
    use agent = runServer ()
    
    let result = Http.RequestString "http://127.0.0.1:45123/hello/"
    
    result |> should equal "get"

[<Test>]
let ``post json body`` () = 
    use agent = runServer ()

    use client = new System.Net.WebClient()
    
    client.Headers.Set(HttpRequestHeader.ContentType, "application/json")
  
    let result = client.UploadString ("http://127.0.0.1:45123/hello/", "[0,1,2]")
    
    result |> should equal "post"

[<Test>]
let ``using unsupported content type``() =
    use agent = runServer ()
        
    use client = new System.Net.WebClient()
        
    client.Headers.Set(HttpRequestHeader.ContentType, "application/xml")      
  
    try 
        client.UploadString ("http://127.0.0.1:45123/hello/", "[0,1,2]") |> ignore
    //failwith "expecting an exception"
    with 
    | :? WebException as ex -> 
        (ex.Response :?> HttpWebResponse).StatusCode |> should equal StatusCode.UnsupportedMediaType

   
[<Test>]
let ``json response``() = 
    use agent = runServer ()
       
    let result = Http.RequestString "http://127.0.0.1:45123/json"
       
    result |> should equal """{"balance":100}"""
   
[<Test>]
let ``mutliple requests``() =              
    use agent = runServer ()
    Http.RequestString "http://127.0.0.1:45123/hello/"  |> ignore        
    
    use client = new System.Net.WebClient()        
    client.Headers.Set(HttpRequestHeader.ContentType, "application/json")
  
    client.UploadString ("http://127.0.0.1:45123/hello/", "[0,1,2]") |> ignore

        
    Http.RequestString "http://127.0.0.1:45123/hello/"  |> ignore
    Http.RequestString "http://127.0.0.1:45123/hello/"  |> ignore
    Http.RequestString "http://127.0.0.1:45123/hello/"  |> ignore