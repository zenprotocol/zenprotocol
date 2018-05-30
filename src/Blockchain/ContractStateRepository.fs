module Blockchain.ContractStateRepository

open Blockchain
open Blockchain.BlockState
open Consensus
open ContractStates
open Types
open UtxoSet
open DataAccess
open DatabaseContext
open FStar

let get (session:Session) contract =
    Collection.tryGet session.context.contractStates session.session contract 
    
let save (session:Session) (states:ContractStates.T) =
    let collection = session.context.contractStates
    let session = session.session

    states
    |> Map.iter (fun contractId ->
        function
        | None ->
            Collection.delete collection session contractId
        | Some contractState ->
            Collection.put collection session contractId contractState)