module Blockchain.TransactionRepository
open Blockchain

open DataAccess
open DatabaseContext
open Consensus.Types

let getOutput (session:Session) outpoint = 
    let tx = Collection.get session.context.transactions session.session outpoint.txHash
    tx.outputs.[outpoint.index |> int32]