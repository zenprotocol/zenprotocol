module Messaging.Events

open Infrastructure.EventBus
open Consensus.Types

type Event = 
    | TransactionAddedToMemPool of Transaction
    
let publish (publisher:Publisher.T<Event>) (event:Event) = 
    Publisher.publish publisher event