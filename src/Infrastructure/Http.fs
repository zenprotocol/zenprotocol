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
    | Get of path : string * query: Map<string,string>
    | Post of path : string * Body

type Context = Request * ReplyFunction

module Server =
    open FStar

    type T =
        { listener : System.Net.HttpListener
          observable : System.IObservable<Context>
          observer : System.IDisposable }
        interface System.IDisposable with
            member x.Dispose() =
                x.observer.Dispose()
                (x.listener :> System.IDisposable).Dispose()

    let private getBody (request : System.Net.HttpListenerRequest) =
        match request.HasEntityBody with
        | false -> Ok None
        | true ->
            let bytes = Array.zeroCreate <| int request.ContentLength64
            request.InputStream.Read(bytes,0, Array.length bytes) |> ignore // TODO: read all

            if request.ContentType.StartsWith HttpContentTypes.Json then
               Ok(Some(request.ContentEncoding.GetString(bytes)))
            else
               Error(sprintf "only %s ContentType is supported" HttpContentTypes.Json)
    

    let private writeTextResponse (response : System.Net.HttpListenerResponse) (code : StatusCode) (text : string) =
        response.StatusCode <- int code
        response.ContentType <- HttpContentTypes.Text
        response.ContentLength64 <- int64 (System.Text.Encoding.UTF8.GetByteCount(text))
        
        try 
            let bytes = System.Text.Encoding.UTF8.GetBytes(text)
            response.OutputStream.Write(bytes, 0, Array.length bytes)
            response.OutputStream.Close()
        with 
        | _ -> ()

    let private writeJsonResponse (response : System.Net.HttpListenerResponse) (code : StatusCode) (json : JsonValue) =
        let stringWriter = new System.IO.StringWriter(System.Globalization.CultureInfo.InvariantCulture)
        json.WriteTo(stringWriter, JsonSaveOptions.DisableFormatting)
        let text = stringWriter.ToString()
        response.StatusCode <- int code
        response.ContentType <- HttpContentTypes.Json
        response.ContentLength64 <- int64 (System.Text.Encoding.UTF8.GetByteCount(text))
        
        try 
            let bytes = System.Text.Encoding.UTF8.GetBytes(text)
            response.OutputStream.Write(bytes, 0, Array.length bytes)
            response.OutputStream.Close()
        with 
        | _ -> ()


    let private writeEmptyResponse (response : System.Net.HttpListenerResponse) (code : StatusCode) =
        try 
            response.StatusCode <- int code
            response.OutputStream.Close()
        with 
        | _ -> ()
        
    let private writeResponse (response : System.Net.HttpListenerResponse) statusCode =
        function
        | TextContent text -> writeTextResponse response statusCode text 
        | JsonContent json ->  writeJsonResponse response statusCode json
        | NoContent ->  writeEmptyResponse response statusCode

    let private invokeOnNext (FsNetMQ.Poller.Poller poller) (observer : System.IObserver<_>) context =
        poller.Run(fun () -> observer.OnNext(context))

    let private removePostfixSlash (path : string) =
        if path.EndsWith("/") then path.Substring(0, path.Length - 1)
        else path

    let private subscribe poller (listener:System.Net.HttpListener) observer =
        let source = new System.Threading.CancellationTokenSource()

        let async () =
            while not source.IsCancellationRequested do
                let context = listener.GetContext()

                let path = removePostfixSlash context.Request.Url.AbsolutePath

                let reply = writeResponse context.Response

                match context.Request.HttpMethod with
                | "GET" ->
                    let nameValueCollection = System.Web.HttpUtility.ParseQueryString context.Request.Url.Query
                    let queryParameters =
                        nameValueCollection.AllKeys
                        |> Seq.map (fun key -> key, nameValueCollection.Get(key))
                        |> Map.ofSeq

                    invokeOnNext poller observer
                        (Get (path, queryParameters), reply)
                | "POST" ->
                    let body = getBody context.Request
                    match body with
                    | Ok body ->
                        invokeOnNext poller observer
                            (Post(path, body), reply)
                    | Error error ->
                        writeTextResponse context.Response StatusCode.UnsupportedMediaType error
                | _ ->
                        writeEmptyResponse context.Response StatusCode.MethodNotAllowed
        
        let task = new System.Threading.Tasks.Task(async, System.Threading.Tasks.TaskCreationOptions.LongRunning)
        task.Start()
        task.ContinueWith(fun task ->
            if task.IsFaulted then 
                System.Environment.FailFast("http thread crashed", task.Exception.Flatten().InnerException) 
            ) |> ignore
    
             
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
