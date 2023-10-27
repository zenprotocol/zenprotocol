module Blockchain.EffectsWriter

open Infrastructure.Writer
open Infrastructure.EventBus
open Infrastructure.ServiceBus.Client
open Messaging.Events
open Messaging.Services

type Effect =
    | EventEffect of Event
    | NetworkCommand of Network.Command

type EffectsWriter<'a> = Writer<Effect, 'a>

let createEffect effect = Writer([ effect ], ())

let publish (event: Event) = createEffect (EventEffect event)

let wrapNetworkCommand cmd = createEffect (NetworkCommand cmd)

let sendMemPool peerId txHashes =
    Network.SendMemPool(peerId, txHashes) |> wrapNetworkCommand

let sendTransactions peerId txs =
    Network.SendTransactions(peerId, txs) |> wrapNetworkCommand

let getTransactions peerId txHashes =
    Network.GetTransactions(peerId, txHashes) |> wrapNetworkCommand

let getBlock blockHash =
    Network.GetBlock blockHash |> wrapNetworkCommand

let getBlockFrom peerId blockHash =
    Network.GetBlockFrom(peerId, blockHash) |> wrapNetworkCommand

let sendTip peerId blockHeader =
    Network.SendTip(peerId, blockHeader) |> wrapNetworkCommand

let sendBlock peerId block =
    Network.SendBlock(peerId, block) |> wrapNetworkCommand

let publishBlock blockHeader =
    Network.PublishBlock blockHeader |> wrapNetworkCommand

let sendHeaders peerId headers =
    Network.SendHeaders(peerId, headers) |> wrapNetworkCommand

let getHeaders peerId startHash endHash =
    Network.GetHeaders(peerId, startHash, endHash) |> wrapNetworkCommand

let disconnectPeer peerId =
    Network.DisconnectPeer peerId |> wrapNetworkCommand

let getTipsFromAllPeers = Network.GetTipFromAllPeers |> wrapNetworkCommand

let run (Writer(effects, x)) publisher client =
    effects
    |> List.iter (function
        | EventEffect event -> Publisher.publish publisher event
        | NetworkCommand command -> Command.send client Network.serviceName command)

    x

let effectsWriter = WriterBuilder<Effect>()
