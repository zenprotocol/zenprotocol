module Messaging.Events

open Infrastructure.EventBus
open Consensus
open Consensus.Types

type Event = 
    | TransactionAddedToMemPool of txHash:Hash.Hash * Transaction
    
let publish (publisher:Publisher.T<Event>) (event:Event) = 
    Publisher.publish publisher event