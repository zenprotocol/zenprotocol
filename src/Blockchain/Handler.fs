module Blockchain.Handler 

open Infrastructure
open Infrastructure.Writer
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus
open Blockchain
open Blockchain.EffectsWriter

let handleCommand command (utxoSet, mempool, orphanPool, acs) =   
    match command with
    | ValidateTransaction tx -> TransactionHandler.validateTransaction tx (utxoSet, mempool, orphanPool, acs)
                                                     
let handleRequest request reply (utxoSet, mempool, orphanPool, acs) = 
    ret (utxoSet,mempool, orphanPool, acs)
    
let handleEvent event (utxoSet, mempool, orphanPool, acs) = 
    ret (utxoSet,mempool, orphanPool, acs)
    