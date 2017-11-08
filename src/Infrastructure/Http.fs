module Infrastructure.Http

open FSharp.Control.Reactive
open FSharp.Data

type StatusCode = System.Net.HttpStatusCode

type Body = string option

type Content = 
    | TextContent of string
    | JsonContent of JsonValue
    | NoContent

type ReplyFunction = StatusCode -> Content -> unit

type Request =
    | Get of path : string * query : string 
    | Post of path : string * Body
    
type Context = Request * ReplyFunction    

module Server = 
    
    type T = 
        { listener : System.Net.HttpListener
          observable : System.IObservable<Context>
          observer : System.IDisposable }
        interface System.IDisposable with
            member x.Dispose() = 
                x.observer.Dispose()
                (x.listener :> System.IDisposable).Dispose()
    
    let private getBody (request : System.Net.HttpListenerRequest) = 
        async { 
            match request.HasEntityBody with
            | false -> return Ok None
            | true -> 
                let! bytes = request.InputStream.AsyncRead(int request.ContentLength64)
                match request.ContentType with
                | HttpContentTypes.Json -> return Ok(Some(request.ContentEncoding.GetString(bytes)))
                | _ -> return Error(sprintf "only %s ContentType is supported" HttpContentTypes.Json)
        }
    
    let private writeTextResponse (response : System.Net.HttpListenerResponse) (code : StatusCode) (text : string) = 
        response.StatusCode <- int code
        response.ContentType <- HttpContentTypes.Text
        response.ContentLength64 <- int64 (System.Text.Encoding.UTF8.GetByteCount(text))
        async { 
            do! response.OutputStream.AsyncWrite(System.Text.Encoding.UTF8.GetBytes(text))
            response.OutputStream.Close()
        }
    
    let private writeJsonResponse (response : System.Net.HttpListenerResponse) (code : StatusCode) (json : JsonValue) = 
        let stringWriter = new System.IO.StringWriter()
        json.WriteTo(stringWriter, JsonSaveOptions.DisableFormatting)
        let text = stringWriter.ToString()
        response.StatusCode <- int code
        response.ContentType <- HttpContentTypes.Json
        response.ContentLength64 <- int64 (System.Text.Encoding.UTF8.GetByteCount(text))
        async { 
            do! response.OutputStream.AsyncWrite(System.Text.Encoding.UTF8.GetBytes(text))
            response.OutputStream.Close()
        }
    
    let private writeEmptyResponse (response : System.Net.HttpListenerResponse) (code : StatusCode) = 
        async { 
            response.StatusCode <- int code
            response.OutputStream.Close()
            return ()
        }
    
    let private writeResponse (response : System.Net.HttpListenerResponse) statusCode = 
        function 
        | TextContent text -> Async.Start(writeTextResponse response statusCode text) |> ignore
        | JsonContent json -> Async.Start(writeJsonResponse response statusCode json) |> ignore
        | NoContent -> Async.Start(writeEmptyResponse response statusCode)
    
    let private invokeOnNext (FsNetMQ.Poller.Poller poller) (observer : System.IObserver<_>) context = 
        poller.Run(fun () -> observer.OnNext(context))
    
    let private removePostfixSlash (path : string) = 
        if path.EndsWith("/") then path.Substring(0, path.Length - 1)
        else path
        
    let private subscribe poller (listener:System.Net.HttpListener) observer =  
        let source = new System.Threading.CancellationTokenSource()
                                             
        let async = 
            async { 
                while true do
                    let! context = Async.AwaitTask(listener.GetContextAsync())
                    
                    let path = removePostfixSlash context.Request.Url.AbsolutePath
                    
                    let reply = writeResponse context.Response
                    
                    match context.Request.HttpMethod with
                    | "GET" ->                     
                        invokeOnNext poller observer 
                            (Get (path, context.Request.Url.Query), reply)
                    | "POST" -> 
                        let! body = getBody context.Request
                        match body with
                        | Ok body -> 
                            invokeOnNext poller observer 
                                (Post(path, body), reply)
                        | Error error -> 
                            do! writeTextResponse context.Response 
                                 StatusCode.UnsupportedMediaType error
                    | _ -> 
                        do! writeEmptyResponse context.Response StatusCode.MethodNotAllowed
            }
        Async.Start(async, source.Token)
        { new System.IDisposable with
           member this.Dispose() = source.Cancel() }
    
    let create poller address = 
        let listener = new System.Net.HttpListener()
        listener.Prefixes.Add(sprintf "http://%s/" address)
        let observable = 
            Observable.publish 
                { 
                    new System.IObservable<_> with
                        member this.Subscribe(observer : System.IObserver<_>) = subscribe poller listener observer 
                }
                                         
        listener.Start()
        let observer = Observable.connect observable
        { listener = listener
          observable = observable
          observer = observer }
    
    let observable (agent : T) = agent.observable
