module Blockchain.EffectsWriter 

open Infrastructure.Writer
open Infrastructure.EventBus
open Messaging.Events

type EffectsWriter<'a> = Writer<'a, Event>

let publish (event:Event) = Writer([event],())   

let run (Writer (events, x)) publisher = 
    List.iter (Publisher.publish publisher) events 
    
    x 
    
let effectsWriter = new WriterBuilder<Event>()    