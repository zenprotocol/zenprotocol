module Blockchain.EffectsWriter

open System.Globalization
open Consensus
open Infrastructure.Writer
open Infrastructure.EventBus
open Infrastructure.ServiceBus.Client
open Messaging.Events
open Messaging.Services

type Effect =
    | EventEffect of Event
    | NetworkCommand of Network.Command

type EffectsWriter<'a> = Writer<'a, Effect>

let publish (event:Event) = Writer([EventEffect event],())

let sendMemPool peerId txHashes =
    let command = Network.SendMemPool (peerId, txHashes)
    Writer([NetworkCommand command],())

let sendTransactions peerId txs =
    let command = Network.SendTransactions (peerId, txs)
    Writer([NetworkCommand command],())

let getTransactions peerId txHashes =
    let command = Network.GetTransactions (peerId, txHashes)
    Writer([NetworkCommand command],())

let getBlock blockHash =
    let command = Network.GetBlock blockHash
    Writer([NetworkCommand command],())

let getBlockFrom peerId blockHash =
    let command = Network.GetBlockFrom (peerId,blockHash)
    Writer([NetworkCommand command],())

let sendTip peerId blockHeader =
    let command = Network.SendTip (peerId, blockHeader)
    Writer([NetworkCommand command],())

let sendBlock peerId block =
    let command = Network.SendBlock (peerId, block)
    Writer([NetworkCommand command],())

let publishBlock blockHeader =
    let command = Network.PublishBlock blockHeader
    Writer([NetworkCommand command],())

let sendHeaders peerId headers =
    let command = Network.SendHeaders (peerId, headers)
    Writer([NetworkCommand command],())

let getHeaders peerId startHash endHash =
    let command = Network.GetHeaders (peerId, startHash, endHash)
    Writer([NetworkCommand command],())

let disconnectPeer peerId =
    let command = Network.DisconnectPeer peerId
    Writer([NetworkCommand command],())

let getTipsFromAllPeers =
    let command = Network.GetTipFromAllPeers
    Writer([NetworkCommand command],())

let run (Writer (effects, x)) publisher client =
    List.iter (function
        | EventEffect event -> Publisher.publish publisher event
        | NetworkCommand command ->
            Command.send client Network.serviceName command
        ) effects

    x

let effectsWriter = new WriterBuilder<Effect>()