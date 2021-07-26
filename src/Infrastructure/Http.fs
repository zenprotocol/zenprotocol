module Infrastructure.Http

open FSharp.Control.Reactive
open FSharp.Data

type StatusCode = System.Net.HttpStatusCode

type Body = string option

type Origin =
    | Any
    | Custom of string
    | No

type Content =
    | TextContent of string
    | JsonContent of JsonValue
    | NoContent

type ReplyFunction = StatusCode -> Content -> unit
type Query = Map<string,string>

type Request =
    | Get of path : string * query : Query
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

    let private getBody (request : System.Net.HttpListenerRequest) : Result<string option,string> =
        if request.HasEntityBody then
            if request.ContentType <> null && request.ContentType.StartsWith HttpContentTypes.Json then
               use reader = new System.IO.StreamReader(request.InputStream,request.ContentEncoding)
                
               reader.ReadToEnd()
               |> Some
               |> Ok
            else
               sprintf "only %s ContentType is supported" HttpContentTypes.Json
               |> Error
        else Ok None

    let private writeEmptyResponse (response : System.Net.HttpListenerResponse) (code : StatusCode) =
        response.StatusCode <- int code
        response.OutputStream.Close()

    let private handleResponse
        (contentType : string )
        (response : System.Net.HttpListenerResponse)
        (code : StatusCode)
        (text : string)
        : unit =
            try
                response.StatusCode <- int code
                response.ContentType <- contentType
                response.ContentLength64 <- int64 (System.Text.Encoding.UTF8.GetByteCount(text))
                let bytes = System.Text.Encoding.UTF8.GetBytes(text)
                response.OutputStream.Write(bytes, 0, Array.length bytes)
                response.OutputStream.Close()
            with _ ->
                if response <> null then 
                   writeEmptyResponse response StatusCode.InternalServerError
    
    let private writeTextResponse
        (response : System.Net.HttpListenerResponse)
        (code : StatusCode)
        (text : string)
        : unit =
            handleResponse HttpContentTypes.Text response code text

    let private writeJsonResponse
        (response : System.Net.HttpListenerResponse)
        (code : StatusCode)
        (json : JsonValue)
        : unit =
            let stringWriter = new System.IO.StringWriter(System.Globalization.CultureInfo.InvariantCulture)
            json.WriteTo(stringWriter, JsonSaveOptions.DisableFormatting)
            stringWriter.ToString()
            |> handleResponse HttpContentTypes.Json response code

       
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
    let private addCORS (context:System.Net.HttpListenerContext) =
        context.Response.AddHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
        context.Response.AddHeader("Access-Control-Allow-Methods", "GET, POST")
        context.Response.AddHeader("Access-Control-Max-Age", "1728000")

    let private subscribe poller (origin:Origin) (listener:System.Net.HttpListener) observer =
        let source = new System.Threading.CancellationTokenSource()
        Async.Start(
            async {
                while not source.IsCancellationRequested || listener.IsListening do
                try
                    let! context = Async.FromBeginEnd(listener.BeginGetContext, listener.EndGetContext)

                    let path = removePostfixSlash context.Request.Url.AbsolutePath
                    match origin with
                    | Any -> 
                        context.Response.AppendHeader("Access-Control-Allow-Origin", "*" )
                    | Custom s ->
                        context.Response.AppendHeader("Access-Control-Allow-Origin", s)
                    | No ->
                        ()

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
                    | "OPTIONS" ->
                            match origin with
                            | No ->
                                ()
                            | _ ->
                                addCORS context
                            writeEmptyResponse context.Response StatusCode.Accepted
                        
                    | _ ->
                            writeEmptyResponse context.Response StatusCode.MethodNotAllowed
                with
                | error ->
                    sprintf "http thread crashed message: %s in: %s stacktrace: %s %s" error.Message error.Source error.StackTrace error.HelpLink
                    |> System.Environment.FailFast
            }, source.Token)
             
        { new System.IDisposable with
           member this.Dispose() = source.Cancel() }

    let create poller origin address =
        let listener = new System.Net.HttpListener()
        listener.Prefixes.Add(sprintf "http://%s/" address)
        let observable =
            Observable.publish
                {
                    new System.IObservable<_> with
                        member this.Subscribe(observer : System.IObserver<_>) = subscribe poller origin listener observer
                }

        listener.Start()
        let observer = Observable.connect observable
        { listener = listener
          observable = observable
          observer = observer }

    let observable (agent : T) = agent.observable
