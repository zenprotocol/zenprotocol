module Blockchain.TransactionRepository
open Blockchain

open DataAccess
open DatabaseContext
open Consensus.Types

let getOutput (session:Session) outpoint = 
    let tx = Collection.get session.context.transactions session.session outpoint.txHash
    tx.outputs.[outpoint.index |> int32]
    
// Check if the transaction is part of the main chain    
let isPartOfMainChain (session:Session) txHash = 
    let blockHashes = MultiCollection.get session.context.transactionBlocks session.session txHash
    
    List.exists (fun blockHash ->
        let header = Collection.get session.context.blocks session.session blockHash
        header.status = ExtendedBlockHeader.MainChain) blockHashes     