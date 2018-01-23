module Blockchain.UtxoSetRepository
open Blockchain

open Consensus
open Consensus.UtxoSet
open DataAccess
open DatabaseContext

let tryGetOutput (session:Session) outpoint = 
    Collection.tryGet session.context.utxoSet session.session outpoint

let save (session:Session) set = 
    let collection = session.context.utxoSet
    let session = session.session

    Map.iter (fun outpoint outputStatus ->
        match outputStatus with
        | Removed -> Collection.delete collection session outpoint
        | outputStatus -> Collection.put collection session outpoint outputStatus) set        
        