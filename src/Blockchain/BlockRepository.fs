module Blockchain.BlockRepository

open Consensus
open Types
open DataAccess
open DatabaseContext
open BlockState

open Blockchain.Serialization

let contains session blockHash =
    Collection.containsKey session.context.blocks session.session blockHash

let tryGetHeader session hash =
    Collection.tryGet session.context.blocks session.session hash

let getHeader session hash =
    Collection.get session.context.blocks session.session hash

let getAllMainHeader session =
    Collection.getAll session.context.blocks session.session
    |> List.filter (fun x -> x.status = ExtendedBlockHeader.BlockStatus.MainChain)
    |> List.sortBy (fun x -> x.header.blockNumber)
    
let getMainHeaderFrom session blockNumber =
    getAllMainHeader session
    |> List.rev
    |> fun xs -> if List.length xs <= blockNumber then [] else List.skip blockNumber xs

let getMainHeaderPaginate session blockNumber take =
    getMainHeaderFrom session blockNumber
    |> List.truncate take

let saveHeader session (block:ExtendedBlockHeader.T) =
    Collection.put session.context.blocks session.session block.hash block

let getFullBlock session (block:ExtendedBlockHeader.T) =
    let transactions =
        Collection.get session.context.blockTransactions session.session block.hash
        |> Seq.map (Collection.get session.context.transactions session.session)
        |> Seq.toList

    {
        header=block.header
        txMerkleRoot=block.txMerkleRoot
        witnessMerkleRoot=block.witnessMerkleRoot
        activeContractSetMerkleRoot=block.activeContractSetMerkleRoot
        commitments=block.commitments
        transactions=transactions
    }

let saveFullBlock session blockHash (block:Block) =
    // Save all transactions
    List.iter (fun ex -> Collection.put session.context.transactions session.session ex.txHash ex) block.transactions

    // Save reference from block to transactions
    List.map (fun ex->ex.txHash) block.transactions
    |> List.toSeq
    |> Collection.put session.context.blockTransactions session.session blockHash

    // Save reference from transactions to block
    List.iter (fun ex ->
        MultiCollection.put session.context.transactionBlocks session.session ex.txHash blockHash) block.transactions

let saveBlockState (session : Session) (blockHash : Hash.Hash) (blockState : BlockState.T) : unit =
    Collection.put session.context.blockState session.session blockHash blockState

let getBlockState session blockHash =
    Collection.get session.context.blockState session.session blockHash

let getBlockChildren session (block:ExtendedBlockHeader.T) =
    Index.getAll session.context.blockChildrenIndex session.session block.hash

let saveGenesisHash session genesisHash =
    SingleValue.put session.context.genesis session.session genesisHash

let tryGetGenesisHeader session =
    match SingleValue.tryGet session.context.genesis session.session with
    | Some blockHash ->
        let header = getHeader session blockHash
        Some header
    | None -> None

let tryGetTip session =
    match SingleValue.tryGet session.context.tip session.session with
    | Some blockHash ->
        let header = getHeader session blockHash
        let blockState = getBlockState session blockHash

        Some (header,blockState.ema,blockState.cgp)
    | None -> None

let tryGetJustTip session =
    SingleValue.tryGet session.context.tip session.session

let updateTip session blockHash =
    SingleValue.put session.context.tip session.session blockHash
