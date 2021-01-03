module Api.Tests.Config

open Consensus
open Infrastructure.Http
open Infrastructure.ServiceBus
open Api.BlockTemplateCache
open FsUnit


let testPassword = "1234"

let private compare (a:Option<'a>) (b:'a) =
    match a with
    | Some a ->
        should equal a b
    | None ->
        ()

let private reply (providedContent: Option<Content>) (providedStatus: Option<StatusCode>) : StatusCode -> Content -> unit =
    fun actualStatus actualContent ->
        
        compare providedStatus actualStatus
        
        compare providedContent actualContent

let private replyNegative (providedContent: Option<Content>) (providedStatus: Option<StatusCode>) : StatusCode -> Content -> unit =
        fun status content ->
        
        compare providedStatus status
        
        compare providedContent content
let private customConfig (c: Option<Content>) (s: Option<StatusCode>) r: Api.Config.Config =
    let reply = r |> Option.defaultValue reply
    let client = Client.create Utils.busName
    {
        reply = reply c s
        replyError = fun error -> reply c s StatusCode.BadRequest (TextContent error) 
        client = client
        chain = Chain.Local
        templateCache = BlockTemplateCache client
    }

let oKConfig content =
    customConfig (Some content) (Some StatusCode.OK) None
    
let contentConfig (c: Content) =
    customConfig (Some c) (Some StatusCode.OK) None

let debugConfig (c: Content) =
    customConfig (Some c) None None

let badRequestConfig (c: Content) =
    customConfig (Some c) (Some StatusCode.BadRequest) None
    
let config =
    customConfig None None None
    
let nonEmptyListConfig =
    customConfig None None (Some replyNegative)
