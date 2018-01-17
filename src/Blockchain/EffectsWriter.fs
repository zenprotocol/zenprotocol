module Blockchain.EffectsWriter 

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
    
let sendTransaction peerId tx = 
    let command = Network.SendTransaction (peerId, tx)
    Writer([NetworkCommand command],())       

let getTransaction peerId txHash = 
    let command = Network.GetTransaction (peerId, txHash)
    Writer([NetworkCommand command],())
    
let getBlock blockHash = 
    let command = Network.GetBlock blockHash
    Writer([NetworkCommand command],())

let getNewBlock peerId blockHash = 
    let command = Network.GetNewBlock (peerId, blockHash)
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


let run (Writer (effects, x)) publisher client = 
    List.iter (function
        | EventEffect event -> Publisher.publish publisher event
        | NetworkCommand command -> Command.send client Network.serviceName command 
        ) effects 
    
    x 
    
let effectsWriter = new WriterBuilder<Effect>()    