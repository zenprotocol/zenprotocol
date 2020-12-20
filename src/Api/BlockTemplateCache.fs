module Api.BlockTemplateCache

open FSharp.Data
open Consensus
open Serialization
open Infrastructure
open Http
open Api.Types
open Messaging.Services
open Logary.Message
open Hash

[<Literal>]
let BlockTemplateCacheSize = 500

type BlockTemplateCache(client) =
    

    let mutable templateList : (Hash * Types.Block * Content) list = []
    let mutable newBlockSeen = true
    let getTemplate hash =
        match Blockchain.tryGetBlockTemplate client hash with
        | None ->
            eventX "GetBlockTemplate timedout"
            |> Log.warning
            None
        | Some block ->
            let bytes = Block.serialize block

            // 100 bytes are the header
            let header,body = Array.splitAt Block.HeaderSize bytes

            let parent = Hash.toString block.header.parent
            let target = Difficulty.uncompress block.header.difficulty |> Hash.toString
            let header = FsBech32.Base16.encode header
            let body = FsBech32.Base16.encode body

            let newResponse =
                BlockTemplateJson.Root(header, body, target, parent, block.header.blockNumber |> int)
                |> fun x -> x.JsonValue
                |> JsonContent

            Some (block, newResponse)
    member this.templates
        with get() = templateList
    static member val timestamp : Timestamp.Timestamp = Timestamp.now()
        with get, set
    member this.stale
        with get() =
            newBlockSeen || (Timestamp.now() - BlockTemplateCache.timestamp > 10_000UL)
    member this.response(hash:Hash) =
        if this.stale
        then
            match getTemplate hash with
            | Some (block, response) ->
                templateList <- [(hash, block, response)]
                newBlockSeen <- false
                BlockTemplateCache.timestamp <- Timestamp.now()
                Some response
            | None ->
                templateList <- []
                None
        else
            match List.tryFind (fun (h,_,_) -> h = hash) templateList with
            | None ->
                match getTemplate hash with
                | Some (block, response) ->
                    templateList <- List.truncate BlockTemplateCacheSize <| (hash, block, response) :: templateList
                    Some response
                | None -> None
            | Some (_, _, response ) -> Some response
    member this.newTip() =
        newBlockSeen <- true
