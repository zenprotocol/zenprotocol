module Messaging.Events

open Infrastructure.EventBus
open Consensus
open Consensus.Types

type Event = 
    | TransactionAddedToMemPool of txHash:Hash.Hash * Transaction
    | BlockAdded of (Hash.Hash * Block)
    | BlockRemoved of (Hash.Hash * Block)
    | TipChanged of BlockHeader