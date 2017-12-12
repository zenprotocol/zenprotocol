module Blockchain.Handler 

open Infrastructure
open Infrastructure.Writer
open Messaging.Services.Blockchain
open Messaging.Events
open Consensus
open Blockchain
open Blockchain.EffectsWriter

let handleCommand command (utxoSet, mempool, orphanPool) =   
    match command with
    | ValidateTransaction tx -> TransactionHandler.validateTransaction tx (utxoSet, mempool, orphanPool)
                                                     
let handleRequest request reply (utxoSet, mempool, orphanPool) = 
    ret (utxoSet,mempool, orphanPool)
    
let handleEvent event (utxoSet, mempool, orphanPool) = 
    ret (utxoSet,mempool, orphanPool)
    