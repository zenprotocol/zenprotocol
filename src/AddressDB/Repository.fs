module AddressDB.Repository

open Blockchain.Tally.Types
open Consensus
open Types
open Wallet.Types
open Hash
open Logary.Message
open Infrastructure
open Messaging.Services
open Messaging.Services.Wallet
open AddressDB
open Consensus.Types
open Consensus.Types
open Consensus.Types
open Result

type Action =
    | UndoBlock
    | AddBlock

type SyncAction = Action * Block

let empty =
    {
        blockHash = Hash.zero
        blockNumber = 0ul
    }
module Input =

    let private update op dataAccess session input status =
        match input with
         | Outpoint outpoint ->
            match DataAccess.OutpointOutputs.tryGet dataAccess session outpoint with
            | Some output ->
                { output with status = status }
                |> DataAccess.OutpointOutputs.put dataAccess session outpoint
            | None ->
                match op with
                | Add ->
                    failwithf "AddressDB could not resolve outpoint"
                | Remove ->
                    ()
         | _ -> ()


    let updateAll op dataAccess session inputs txHash confirmationStatus =
        for input in inputs do
            match op with
            | Add ->
                Spent (txHash, confirmationStatus)
            | Remove ->
                Unspent
            |> update op dataAccess session input

module Output =
    /// the iterator is used to handle the the update of a block, when added we work on DBOutput
    /// while when we remove we map the output to outpoint to remove from the database
    type private Iterator =
        | DBOutput of DBOutput
        | Outpoint of Outpoint

    let private update dataAccess session outputIterator =
        match outputIterator with
        | DBOutput dbOutput ->
            DataAccess.OutpointOutputs.put dataAccess session dbOutput.outpoint dbOutput
            DataAccess.AddressOutpoints.put dataAccess session dbOutput.address dbOutput.outpoint
        | Outpoint outpoint ->
            match DataAccess.OutpointOutputs.tryGet dataAccess session outpoint with
            | Some output ->
                DataAccess.OutpointOutputs.delete dataAccess session outpoint
                DataAccess.AddressOutpoints.delete dataAccess session output.address outpoint
            | None ->
                ()

    let private mapOutputs op outputs txHash confirmationStatus =
        match op with
        | Add ->
            confirmationStatus
            |> View.mapUnspentTxOutputs outputs txHash
            |> List.map DBOutput
        | Remove ->
            outputs
            |> List.mapi (fun index _ -> Outpoint {txHash = txHash; index = uint32 index})


    let updateAll op dataAccess session outputs txHash confirmationStatus =

        let outputs = mapOutputs op outputs txHash confirmationStatus

        for output in outputs do
            update dataAccess session output


module Witness =

    let updateAll op dataAccess session witnesses txHash confirmationStatus  =

        for witness in witnesses do
            match witness with
            | ContractWitness cw ->
                let witnessPoint = View.witnessPoint txHash witnesses cw

                match op with
                | Add ->
                    DataAccess.ContractData.put dataAccess session witnessPoint (cw.command, cw.messageBody)
                    DataAccess.ContractHistory.put dataAccess session cw.contractId witnessPoint
                    DataAccess.ContractConfirmations.put dataAccess session witnessPoint confirmationStatus
                | Remove ->
                    DataAccess.ContractData.delete dataAccess session witnessPoint
                    DataAccess.ContractHistory.delete dataAccess session cw.contractId witnessPoint
                    DataAccess.ContractConfirmations.delete dataAccess session witnessPoint
            | _ ->
                ()
module Mint =

    let updateAll op dataAccess session tx =

        for input in tx.inputs do
            match input with
            | Mint spend ->
                match op with
                | Add ->
                    tx.witnesses
                    |> List.choose (fun x -> match x with | ContractWitness cw -> Some (cw.command, cw.messageBody) | _ -> None)
                    |> List.head //as we have  mint then surely we have a cw
                    |> DataAccess.ContractAssets.put dataAccess session spend.asset
                | Remove ->
                    DataAccess.ContractAssets.delete dataAccess session spend.asset
            | _ -> ()
                    
module Block =

    let private transactions (op : UpdateOperation) block =

        let blockHash = Block.hash block.header

        match op with
        | Add ->
            block.transactions
            |> List.mapi (fun index ex -> Confirmed (block.header.blockNumber, blockHash, index),ex)
        | Remove ->
            block.transactions
            |> List.rev
            |> List.map (fun ex -> Unconfirmed, ex)

    let update op dataAccess session block =

        lazy

        for (confirmationStatus,ex) in transactions op block do
             Input  .updateAll op dataAccess session ex.tx.inputs    ex.txHash confirmationStatus
             Output .updateAll op dataAccess session ex.tx.outputs   ex.txHash confirmationStatus
             Witness.updateAll op dataAccess session ex.tx.witnesses ex.txHash confirmationStatus
             Mint   .updateAll op dataAccess session ex.tx

module Tip =

    let update op dataAccess session block =

        lazy

        match op with
        | Add ->
            {blockNumber = block.header.blockNumber; blockHash = (Block.hash block.header)}
        | Remove ->
            {blockNumber = block.header.blockNumber - 1ul; blockHash = block.header.parent}
        |> DataAccess.Tip.put dataAccess session


let log op block =

    eventX "AddressDB {operation} block #{blockNumber} of block {blockHash}"
    >> setField "operation" (match op with | Add -> "adding" | Remove -> "removing")
    >> setField "blockNumber" block.header.blockNumber
    >> setField "blockHash" (Hash.toString (Block.hash block.header))
    |> Log.info

let updateBlock (op:UpdateOperation) dataAccess session (block:Block) : unit =
    let tipHash = DataAccess.Tip.tryGet dataAccess session |> Option.defaultValue empty

    match op with
    | Add ->
        if tipHash.blockHash <> block.header.parent then
            failwithf "trying to add a block to the tip but the tip is in different chain"
    | Remove ->
        if tipHash.blockHash <> Block.hash block.header then
            failwithf "trying to remove a block from the tip but the tip is in different chain"

    seq {
        yield Block.update op dataAccess session block
        yield Tip  .update op dataAccess session block
    }
    |> Seq.iter (fun x -> x.Force())

    log op block


let addBlock =
    updateBlock Add

let undoBlock =
    updateBlock Remove

let sync dataAccess session tipBlockHash (tipHeader:BlockHeader) (headers: Map<Hash.Hash,Block>) =
    let account = DataAccess.Tip.get dataAccess session
    // Find the fork block of the account and the blockchain, logging actions
    // to perform. Undo each block in the account's chain but not the blockchain,
    // and add each block in the blockchain but not the account's chain.
    let getBlock hash = headers |> Map.find hash

    let rec locate ((x,i),(y,j)) acc =

        if x = y && i = j then acc
        elif i > j
        then
            locate (((getBlock x).header.parent, i-1ul), (y,j))
                ((AddBlock, getBlock x) :: acc)
        elif i < j
        then
            locate ((x,i), ((getBlock y).header.parent, j-1ul))
                ((UndoBlock, getBlock y) :: acc)
        else
            locate (((getBlock x).header.parent, i-1ul), ((getBlock y).header.parent, j-1ul))
                ((AddBlock, getBlock x) :: (UndoBlock, getBlock y) :: acc)

    let actions = locate ((tipBlockHash, tipHeader.blockNumber),
                          (account.blockHash, account.blockNumber)) []

    let toUndo, toAdd = List.partition (function | UndoBlock, _ -> true | _ -> false) actions
    let toUndo = List.rev toUndo     // Blocks to be undo were found backwards.
    let sortedActions = toUndo @ toAdd

    sortedActions
    |> List.iter (function
        | UndoBlock, block -> undoBlock dataAccess session block
        | AddBlock, block -> addBlock dataAccess session block
        )

let init dataAccess session =
    DataAccess.Tip.put dataAccess session empty

let reset dataAccess session =
    DataAccess.Tip.put dataAccess session empty

    DataAccess.OutpointOutputs.truncate dataAccess session
    DataAccess.AddressOutpoints.truncate dataAccess session
    DataAccess.ContractData.truncate dataAccess session
    DataAccess.ContractConfirmations.truncate dataAccess session
