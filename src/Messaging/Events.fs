module Messaging.Events

open Infrastructure.EventBus
open Consensus
open Consensus.Types

type Event = 
    | TransactionAddedToMemPool of txHash:Hash.Hash * Transaction   