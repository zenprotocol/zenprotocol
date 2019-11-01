module AddressDB.Repository

open Consensus
open Types
open Wallet.Types
open Hash
open Logary.Message
open Infrastructure
open Messaging.Services
open Messaging.Services.Wallet
open AddressDB
open Result

let empty = 
    {
        blockHash = Hash.zero
        blockNumber = 0ul
    }

let getOutputs dataAccess session view mode addresses : PointedOutput list =
    View.AddressOutpoints.get view dataAccess session addresses
    |> View.OutpointOutputs.get view dataAccess session
    |> List.choose (fun dbOutput -> 
        if mode <> UnspentOnly || dbOutput.status = Unspent then 
            Some (dbOutput.outpoint, { spend = dbOutput.spend; lock = dbOutput.lock })
        else
            None)

let getBalance dataAccess session view mode addresses =
    getOutputs dataAccess session view mode addresses 
    |> List.fold (fun balance (_,output) ->
        match Map.tryFind output.spend.asset balance with
        | Some amount -> Map.add output.spend.asset (amount + output.spend.amount) balance
        | None -> Map.add output.spend.asset output.spend.amount balance
    ) Map.empty

let getConfirmations blockNumber =
    function
    | Confirmed (blockNumber',_,blockIndex) ->
        blockNumber - blockNumber' + 1ul, blockIndex
    | Unconfirmed ->
        0ul,0

let getOutputsInfo blockNumber outputs = 

    let incoming = List.map (fun (output:DBOutput) ->
        let txHash = output.outpoint.txHash
        let confirmations,blockIndex = getConfirmations blockNumber output.confirmationStatus

        txHash, output.spend.asset, output.spend.amount |> bigint,confirmations,blockIndex,output.lock) outputs

    let outgoing = List.choose (fun (output:DBOutput) ->
        match output.status with
        | Unspent -> None
        | Spent (txHash,confirmationStatus) ->
            let confirmations,blockIndex = getConfirmations blockNumber confirmationStatus

            (txHash, output.spend.asset, output.spend.amount |> bigint |> (*) -1I,confirmations,blockIndex,output.lock)
            |> Some) outputs

    incoming @ outgoing
    |> List.fold (fun txs (txHash, asset, amount, confirmations, blockIndex, lock) ->
        match Map.tryFind (txHash, asset) txs with
        | None -> Map.add (txHash, asset) (amount, confirmations, blockIndex, lock) txs
        | Some (amount',_,_,lock) -> Map.add (txHash, asset) (amount + amount', confirmations, blockIndex, lock) txs) Map.empty
    |> Map.toSeq
    |> Seq.map (fun ((txHash, asset),(amount, confirmations, blockIndex, lock)) ->
        if amount >= 0I then
            (txHash,TransactionDirection.In, {asset=asset;amount = uint64 amount}, confirmations, blockIndex, lock)
        else
            (txHash,TransactionDirection.Out, {asset=asset;amount = amount * -1I |> uint64}, confirmations, blockIndex, lock))
    |> List.ofSeq

let getTransactionCount dataAccess session view blockNumber addresses =
        let outputs =
            View.AddressOutpoints.get view dataAccess session addresses
            |> View.OutpointOutputs.get view dataAccess session
        List.length (outputs |> getOutputsInfo blockNumber)
        
let getHistory dataAccess session view skip take addresses =
    let account = DataAccess.Tip.get dataAccess session
    
    View.AddressOutpoints.get view dataAccess session addresses
    |> View.OutpointOutputs.get view dataAccess session
    |> getOutputsInfo account.blockNumber
    |> List.sortWith Wallet.Account.txComparer
    |> Wallet.Account.paginate skip take
    |> List.map (fun (txHash,direction,spend,confirmations,_,lock) -> txHash,direction,spend,confirmations,lock)

let getContractHistory dataAccess session view skip take contractId =
    let account = DataAccess.Tip.get dataAccess session

    let comparer a1 a2 =
        let comparer (index1, block1) (index2, block2) = 
            if index1 = index2 then
                block2 - block1
            else        
                int (index2 - index1)
        comparer (snd a1) (snd a2)
    
    View.ContractHistory.get view dataAccess session contractId
    |> List.map (fun witnessPoint ->
        View.ContractData.get view dataAccess session witnessPoint,
        View.ContractConfirmations.get view dataAccess session witnessPoint
        |> getConfirmations account.blockNumber)
    |> List.sortWith comparer
    |> Wallet.Account.paginate skip take
    |> List.map (fun ((command, messageBody, txHash), (confirmations, _)) -> command, messageBody, txHash, confirmations)
    
let private witnessIndex tx =
    List.tryFindIndex (fun witness ->
        match witness with
        | ContractWitness _ -> true
        | _ -> false) tx.witnesses
    |> ofOption "missing contract witness"
    |> get
    |> uint32

let addBlock dataAccess session blockHash block =
    let account = DataAccess.Tip.get dataAccess session

    if account.blockHash = blockHash || block.header.blockNumber < account.blockNumber then
        // we already handled the block, skip
        ()
    elif account.blockHash <> block.header.parent then
        failwithf "trying to add a block to account but account in different chain %A %A" (block.header.blockNumber) (account.blockNumber)
    else
    
    eventX "AddressDB adding block #{blockNumber}"
    >> setField "blockNumber" block.header.blockNumber
    |> Log.info

    block.transactions
    |> List.mapi (fun blockIndex ex -> blockIndex, ex.tx, ex.txHash)
    |> List.iter (fun (blockIndex, tx, txHash) ->
        let confirmationStatus = Confirmed (block.header.blockNumber, blockHash, blockIndex)
        
        confirmationStatus
        |> View.mapTxOutputs tx txHash
        |> List.iter (fun (address, outpoint, output) ->
            DataAccess.OutpointOutputs.put dataAccess session outpoint output
            DataAccess.AddressOutpoints.put dataAccess session address outpoint
        )
        
        tx.inputs
        |> List.iter (function
            | Outpoint outpoint ->
                match DataAccess.OutpointOutputs.tryGet dataAccess session outpoint with
                | Some output ->
                    { output with status = Spent (txHash, confirmationStatus) }
                    |> DataAccess.OutpointOutputs.put dataAccess session outpoint
                | None ->
                    failwithf "AddressDB could not resolve outpoint"
            | _ -> ()
        )
            
        tx.witnesses
        |> List.iter (function 
            | ContractWitness cw -> 
                let witnessPoint = View.witnessPoint txHash tx cw

                DataAccess.ContractData.put dataAccess session witnessPoint (cw.command, cw.messageBody)
                DataAccess.ContractHistory.put dataAccess session cw.contractId witnessPoint
                DataAccess.ContractConfirmations.put dataAccess session witnessPoint confirmationStatus
            | _ -> 
                ()
        )
    )

    { account with blockNumber = block.header.blockNumber; blockHash = blockHash}
    |> DataAccess.Tip.put dataAccess session

let undoBlock dataAccess session blockHash block =
    let account = DataAccess.Tip.get dataAccess session

    if account.blockHash = block.header.parent || block.header.blockNumber > account.blockNumber then
        // we already undo this block, skipping
        ()
    elif account.blockHash <> blockHash then
        failwithf "trying to undo a block to account but account in different chain %A %A" (block.header.blockNumber) (account.blockNumber)
    else

    List.rev block.transactions
    |> List.iter (fun ex ->
        // Unmark outputs as spent
        ex.tx.inputs
        |> List.iter (fun input ->
            match input with
            | Outpoint outpoint ->
                match DataAccess.OutpointOutputs.tryGet dataAccess session outpoint with
                | Some output ->
                    {output with status = Unspent}
                    |> DataAccess.OutpointOutputs.put dataAccess session outpoint
                | None ->
                    ()
            | _ -> ()

        )

        // Delete outputs
        ex.tx.outputs
        |> List.mapi (fun index _ -> {txHash = ex.txHash; index = uint32 index})
        |> List.iter (fun outpoint ->
            match DataAccess.OutpointOutputs.tryGet dataAccess session outpoint with
            | Some output ->
                DataAccess.OutpointOutputs.delete dataAccess session outpoint
                DataAccess.AddressOutpoints.delete dataAccess session output.address outpoint
            | None -> ()
        )
        
        ex.tx.witnesses
        |> List.iter (function 
            | ContractWitness cw ->
                let witnessPoint = View.witnessPoint ex.txHash ex.tx cw
                
                DataAccess.ContractData.delete dataAccess session witnessPoint
                DataAccess.ContractHistory.delete dataAccess session cw.contractId witnessPoint
                DataAccess.ContractConfirmations.delete dataAccess session witnessPoint
            | _ ->
                ()
        )
    )

    {account with blockNumber = block.header.blockNumber - 1ul; blockHash = block.header.parent}
    |> DataAccess.Tip.put dataAccess session

type private SyncAction =
    | Undo of Block * Hash
    | Add of Block * Hash

let sync dataAccess session tipBlockHash (tipHeader:BlockHeader) (getHeader:Hash -> BlockHeader) (getBlock:Hash -> Block) =
    let account = DataAccess.Tip.get dataAccess session

    // Find the fork block of the account and the blockchain, logging actions
    // to perform. Undo each block in the account's chain but not the blockchain,
    // and add each block in the blockchain but not the account's chain.
    let rec locate ((x,i),(y,j)) acc =

        if x = y && i = j then acc
        elif i > j
        then
            locate (((getHeader x).parent, i-1ul), (y,j)) (Add (getBlock x, x) :: acc)
        elif i < j
        then
            locate ((x,i), ((getHeader y).parent, j-1ul)) (Undo (getBlock y, y) :: acc)
        else
            locate (((getHeader x).parent, i-1ul), ((getHeader y).parent, j-1ul)) (Add (getBlock x, x) :: Undo (getBlock y,y) :: acc)

    let actions = locate ((tipBlockHash, tipHeader.blockNumber), (account.blockHash, account.blockNumber)) []

    let toUndo, toAdd = List.partition (function | Undo _ -> true | _ -> false) actions
    let toUndo = List.rev toUndo     // Blocks to be undo were found backwards.
    let sortedActions = toUndo @ toAdd

    sortedActions
    |> List.iter (function
        | Undo (block,hash) -> undoBlock dataAccess session hash block
        | Add (block,hash) -> addBlock dataAccess session hash block)

let init dataAccess session =
    DataAccess.Tip.put dataAccess session empty

let reset dataAccess session =
    DataAccess.Tip.put dataAccess session empty

    DataAccess.OutpointOutputs.truncate dataAccess session
    DataAccess.AddressOutpoints.truncate dataAccess session
    DataAccess.ContractData.truncate dataAccess session
    DataAccess.ContractConfirmations.truncate dataAccess session
    