module Api.Config

open Infrastructure.Http
open Consensus.Chain
open BlockTemplateCache

type Config = {
    reply: ReplyFunction
    replyError: string -> unit
    client: FsNetMQ.Socket.T
    chain: Chain
    templateCache : BlockTemplateCache
    isRemote : bool
}
