module Blockchain.UtxoSetRepository
open Blockchain

open Consensus
open Consensus.Types
open Consensus.UtxoSet
open DataAccess
open DatabaseContext
open FStar

let get (session:Session) outpoint = 
    match Collection.tryGet session.context.utxoSet session.session outpoint with
    | Some x -> x
    | None -> NoOutput

let save (session:Session) set = 
    let collection = session.context.utxoSet
    let contractCollection = session.context.contractUtxo
    let session = session.session
    
    let deleteContractUtxo outpoint = 
        let outputStatus = Collection.tryGet collection session outpoint
        
        match outputStatus with 
        | Some (Unspent output) ->
            match output.lock with
            | Contract cHash ->
                MultiCollection.delete contractCollection session cHash (outpoint,output)               
            | _ -> ()             
        | _ -> ()
        
    let addContractUtxo outpoint output =
        match output.lock with
        | Contract cHash ->
            MultiCollection.put contractCollection session cHash (outpoint,output)
        | _ -> ()         

    Map.iter (fun outpoint outputStatus ->
        match outputStatus with
        | NoOutput -> 
            deleteContractUtxo outpoint                   
            Collection.delete collection session outpoint
        | Spent output ->
            deleteContractUtxo outpoint
            Collection.put collection session outpoint (Spent output)
        | Unspent output -> 
            addContractUtxo outpoint output
            Collection.put collection session outpoint (Unspent output)) set        
        