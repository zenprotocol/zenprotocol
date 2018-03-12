module Blockchain.BlockRepository

open Infrastructure
open Blockchain
open Consensus
open Consensus
open Consensus.Types
open DataAccess
open MBrace.FsPickler.Combinators
open Blockchain.DatabaseContext

let contains session blockHash =
    Collection.containsKey session.context.blocks session.session blockHash

let tryGetHeader session hash =
    Collection.tryGet session.context.blocks session.session hash

let getHeader session hash =
    Collection.get session.context.blocks session.session hash

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
    let transactions =
        block.transactions
        |> List.map Transaction.hash
        |> List.zip block.transactions

    // Save all transactions
    List.iter (fun (tx,txHash) -> Collection.put session.context.transactions session.session txHash tx) transactions

    // Save reference from block to transactions
    List.map snd transactions
    |> List.toSeq
    |> Collection.put session.context.blockTransactions session.session blockHash

    // Save reference from transactions to block
    List.iter (fun (_,txHash) ->
        MultiCollection.put session.context.transactionBlocks session.session txHash blockHash) transactions

let saveBlockState session blockHash (acs:ActiveContractSet.T) ema =
    let blockState =
        {
            ema = ema
            activeContractSet =
                ActiveContractSet.getContracts acs
                |> Seq.map (fun contract -> contract.hash,contract.expiry,contract.size,contract.code)
                |> List.ofSeq
        }

    Collection.put session.context.blockState session.session blockHash blockState

let getBlockState session blockHash =
    let blockState = Collection.get session.context.blockState session.session blockHash

    let getOk (cHash,result) =
        match result with
        | Ok x -> cHash,x
        | Error error -> failwithf "cannot load contract from db due to %A" error

    let acs =
        blockState.activeContractSet
        |> List.map (fun (cHash,expiry,size,code) -> cHash,Contract.load session.context.contractPath expiry size code cHash)
        |> List.map getOk
        |> List.toArray
        |> SparseMerkleTree.addMultiple ActiveContractSet.empty

    acs,blockState.ema

let getBlockChildren session (block:ExtendedBlockHeader.T) =
    Index.getAll session.context.blockChildrenIndex session.session block.hash

let tryGetTip session =
    match SingleValue.tryGet session.context.tip session.session with
    | Some blockHash ->
        let header = getHeader session blockHash
        let acs,ema = getBlockState session blockHash

        Some (header,acs,ema)
    | None -> None

let updateTip session blockHash =
    SingleValue.put session.context.tip session.session blockHash
