module Messaging.Events

open Infrastructure.EventBus
open Consensus
open Consensus.Types

type Event = 
    | TransactionAddedToMemPool of txHash:Hash.Hash * Transaction
    | BlockAdded of Block
    | BlockRemoved of Block
    | TipChanged of BlockHeader   