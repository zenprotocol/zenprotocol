module Blockchain.UtxoSetRepository
open Blockchain

open Consensus
open Consensus.UtxoSet
open DataAccess
open DatabaseContext

let get (session:Session) outpoint = 
    match Collection.tryGet session.context.utxoSet session.session outpoint with
    | Some x -> x
    | None -> NoOuput

let save (session:Session) set = 
    let collection = session.context.utxoSet
    let session = session.session

    Map.iter (fun outpoint outputStatus ->
        match outputStatus with
        | NoOuput -> Collection.delete collection session outpoint
        | outputStatus -> Collection.put collection session outpoint outputStatus) set        
        