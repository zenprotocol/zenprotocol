module Wallet.Account

open Consensus
open Consensus.Crypto
open Consensus.Types
open Consensus.Hash
open Wallet
open Infrastructure.Result
open Wallet
open Wallet.DataAccess
open Wallet.Types
open Messaging.Services.Wallet

// TODO: handle history

let result = new ResultBuilder<string>()

let zenKeyPath = "m/44'/258'/0'/"

let deriveNewAddress index key =
    ExtendedKey.derive 2 key
    >>= ExtendedKey.derive index

let deriveChange index key =
    ExtendedKey.derive 1 key
    >>= ExtendedKey.derive index

let deriveExternal index key =
    ExtendedKey.derive 0 key
    >>= ExtendedKey.derive index

let import dataAccess session mnemonicPhrase password tipHash tipBlockNumber = result {
    let mnemonicPhrase = (String.concat " " mnemonicPhrase)

    let! publicKey =
        ExtendedKey.fromMnemonicPhrase mnemonicPhrase
        >>= ExtendedKey.derivePath zenKeyPath
        >>= ExtendedKey.neuter

    let! externalPKHash =
        deriveExternal 0 publicKey
        >>= ExtendedKey.getPublicKey
        <@> PublicKey.hash

    {pkHash=externalPKHash; addressType=External 0}
    |> DataAccess.Addresses.put dataAccess session externalPKHash

    let! changePKHash =
        deriveChange 0 publicKey
        >>= ExtendedKey.getPublicKey
        <@> PublicKey.hash

    {pkHash=changePKHash; addressType=Change 0}
    |> DataAccess.Addresses.put dataAccess session changePKHash

    let secure = Secured.create password mnemonicPhrase

    let account = {
        blockHash = tipHash
        blockNumber = tipBlockNumber
        counter = 0
        publicKey = publicKey
        secureMnemonicPhrase = secure
        changePKHash = changePKHash
        externalPKHash = externalPKHash
    }

    DataAccess.Account.put dataAccess session account

    return ()
}

let getAddress dataAccess session chain =
    let account = DataAccess.Account.get dataAccess session
    Address.encode chain (Address.PK account.externalPKHash)

let getPKHash dataAccess session =
    let account = DataAccess.Account.get dataAccess session
    account.externalPKHash

let getNewPKHash dataAccess session =
    let account = DataAccess.Account.get dataAccess session
    let index = account.counter
    let account = { account with counter = account.counter + 1 }

    deriveNewAddress index account.publicKey
    |> Result.bind ExtendedKey.getPublicKey
    |> Result.map (fun key ->
        let pkHash = PublicKey.hash key

        let address = {
            pkHash = pkHash
            addressType = Payment index
        }
        DataAccess.Addresses.put dataAccess session pkHash address

        DataAccess.Account.put dataAccess session account

        pkHash, index)

let getNewAddress dataAccess session chain =
    getNewPKHash dataAccess session
    |> Result.map (fun (pkHash, index) -> Address.encode chain (Address.PK pkHash),index)

let importWatchOnlyAddress dataAccess session chain address =
    Address.decodePK chain address
    |> Result.bind (fun pkHash ->
        if DataAccess.Addresses.contains dataAccess session pkHash then
            Error "address already exist"
        else
            let address = {
                pkHash = pkHash
                addressType = WatchOnly
            }

            DataAccess.Addresses.put dataAccess session pkHash address

            Ok ()
    )

let private filterOutputByConfirmations account confirmations output =
    match output.confirmationStatus with
    | Confirmed (blockNumber,_,_) ->
        if confirmations = 0ul || confirmations = 1ul then
            true
        else
            blockNumber <= (account.blockNumber - confirmations) + 1ul
    | Unconfirmed -> confirmations = 0ul


let getReceived dataAccess session view chain confirmations =
    let account = DataAccess.Account.get dataAccess session

    let outputs =
        View.Outputs.getAll view dataAccess session
        |> List.filter (filterOutputByConfirmations account confirmations)

    List.fold (fun received (output:Output) ->
        let address = Address.encode chain (Address.PK output.pkHash)

        let key = (address, output.spend.asset)

        match Map.tryFind key received with
        | Some amount -> Map.add key (amount + output.spend.amount) received
        | None -> Map.add key output.spend.amount received
    ) Map.empty outputs

let getAddressOutputs dataAccess session view chain address =
    let account = DataAccess.Account.get dataAccess session

    let confirmations output =
        match output.confirmationStatus with
        | Confirmed (blockNumber,_,_) -> account.blockNumber - blockNumber + 1ul
        | _ -> 0ul

    let isSpent = function
        | Spent _ -> true
        | _ -> false

    Address.decodePK chain address
        |> Result.map (fun pkHash ->

            View.AddressOutputs.get view dataAccess session pkHash
            |> List.map (fun output -> output.outpoint, output.spend,confirmations output, isSpent output.status))

let private foldToBalance outputs =
    List.fold (fun received (output:Output) ->
        match Map.tryFind output.spend.asset received with
        | Some amount -> Map.add output.spend.asset (amount + output.spend.amount) received
        | None -> Map.add output.spend.asset output.spend.amount received
    ) Map.empty outputs

let getAddressBalance dataAccess session view chain address confirmations =
    Address.decodePK chain address
    |> Result.map (fun pkHash ->
        let account = DataAccess.Account.get dataAccess session

        let outputs =
            View.AddressOutputs.get view dataAccess session pkHash
            |> List.filter (filterOutputByConfirmations account confirmations)

        outputs
        |> List.filter (fun (output:Output) -> output.status = Unspent)
        |> foldToBalance
    )

let getBalance dataAccess session view confirmations =
    let account = DataAccess.Account.get dataAccess session

    let spendableAddresses =
        DataAccess.Addresses.getAll dataAccess session
        |> List.filter (fun address -> address.addressType <> WatchOnly) // Both payment and change addresses
        |> List.map (fun address -> address.pkHash)
        |> Set.ofList

    let outputs =
        View.Outputs.getAll view dataAccess session
        |> List.filter (filterOutputByConfirmations account confirmations)

    outputs
    |> List.filter (fun output -> Set.contains output.pkHash spendableAddresses && output.status = Unspent)
    |> foldToBalance

let getUnspentOutputs dataAccess session view confirmations =
    let account = DataAccess.Account.get dataAccess session

    View.Outputs.getAll view dataAccess session
    |> List.filter (filterOutputByConfirmations account confirmations)
    |> List.map (fun output -> output.outpoint,{lock=output.lock;spend=output.spend})

let addBlock dataAccess session blockHash block =
    let account = DataAccess.Account.get dataAccess session

    if account.blockHash = blockHash || block.header.blockNumber < account.blockNumber then
        // we already handled the block, skip
        ()
    elif account.blockHash <> block.header.parent then
        failwithf "trying to add a block to account but account in different chain %A %A" (block.header.blockNumber) (account.blockNumber)
    else

    block.transactions
    |> List.mapi (fun index tx -> index,tx)
    |> List.iter (fun (blockIndex, tx) ->
        let txHash = Transaction.hash tx

        // Mark transaction inputs as spent
        List.iter (fun input ->
            match input with
            | Outpoint outpoint ->
                match DataAccess.Outputs.tryGet dataAccess session outpoint with
                | Some output ->
                    {output with status = Spent (txHash, Confirmed (block.header.blockNumber, blockHash, blockIndex))}
                    |> DataAccess.Outputs.put dataAccess session outpoint
                | None ->
                    ()
            | _ -> ()

        ) tx.inputs

        // Add new outputs to database
        tx.outputs
        |> List.mapi (fun index output -> uint32 index,output)
        |> List.choose (fun (index,output) ->
            match output.lock with
            | Coinbase (_,pkHash) when (DataAccess.Addresses.contains dataAccess session pkHash) ->
                Some (pkHash,index,output)
            | PK pkHash when (DataAccess.Addresses.contains dataAccess session pkHash) ->
                Some (pkHash,index,output)
            | _ -> None)
        |> List.iter (fun (pkHash,index,output) ->
            let outpoint = {txHash=txHash; index=index}

            let output = {
                pkHash = pkHash
                spend = output.spend
                lock = output.lock
                outpoint = outpoint
                status = Unspent
                confirmationStatus = Confirmed (block.header.blockNumber, blockHash, blockIndex)
            }

            DataAccess.Outputs.put dataAccess session outpoint output

            // Add the new output to the address
            DataAccess.AddressOutputs.put dataAccess session pkHash outpoint
        )
    )

    {account with blockNumber = block.header.blockNumber; blockHash = blockHash}
    |> DataAccess.Account.put dataAccess session

let undoBlock dataAccess session blockHash block =
    let account = DataAccess.Account.get dataAccess session

    if account.blockHash = block.header.parent || block.header.blockNumber > account.blockNumber then
        // we already undo this block, skipping
        ()
    elif account.blockHash <> blockHash then
        failwithf "trying to undo a block to account but account in different chain %A %A" (block.header.blockNumber) (account.blockNumber)
    else

    List.rev block.transactions
    |> List.iter (fun tx ->
        // Unmark outputs as spentg
        List.iter (fun input ->
            match input with
            | Outpoint outpoint ->
                match DataAccess.Outputs.tryGet dataAccess session outpoint with
                | Some output ->
                    {output with status = Unspent}
                    |> DataAccess.Outputs.put dataAccess session outpoint
                | None ->
                    ()
            | _ -> ()

        ) tx.inputs

        let txHash = Transaction.hash tx

        // Delete outputs
        tx.outputs
        |> List.mapi (fun index _ -> uint32 index)
        |> List.iter (fun index ->
            let outpoint = {txHash=txHash; index=index}

            match DataAccess.Outputs.tryGet dataAccess session outpoint with
            | Some output ->
                DataAccess.Outputs.delete dataAccess session outpoint
                DataAccess.AddressOutputs.delete dataAccess session output.pkHash outpoint
            | None -> ()
        )
    )

    {account with blockNumber = block.header.blockNumber - 1ul; blockHash = block.header.parent}
    |> DataAccess.Account.put dataAccess session

type private SyncAction =
    | Undo of Block * Hash
    | Add of Block * Hash

let sync dataAccess session tipBlockHash (tipHeader:BlockHeader) (getHeader:Hash -> BlockHeader) (getBlock:Hash -> Block) =
    let account = DataAccess.Account.get dataAccess session

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


let reset dataAccess session =
    let account = DataAccess.Account.get dataAccess session

    {account with blockHash = Hash.zero; blockNumber = 0ul}
    |> DataAccess.Account.put dataAccess session

    DataAccess.Outputs.truncate dataAccess session
    DataAccess.AddressOutputs.truncate dataAccess session

let sign dataAccess session password path message = result {
    let account = DataAccess.Account.get dataAccess session

    let! secretKey =
            Secured.decrypt password account.secureMnemonicPhrase
            >>= ExtendedKey.fromMnemonicPhrase
            >>= ExtendedKey.derivePath path
            >>= ExtendedKey.getPrivateKey

    return Crypto.sign secretKey message
}

let getPublicKey dataAccess session password path  =
    let account = DataAccess.Account.get dataAccess session

    Secured.decrypt password account.secureMnemonicPhrase
    >>= ExtendedKey.fromMnemonicPhrase
    >>= ExtendedKey.derivePath path
    >>= ExtendedKey.getPublicKey

let checkPassword dataAccess session password =
    let account = DataAccess.Account.get dataAccess session

    Secured.decrypt password account.secureMnemonicPhrase
    |> Result.map ignore

let getMnemonicPhrase dataAccess session password =
    let account = DataAccess.Account.get dataAccess session

    Secured.decrypt password account.secureMnemonicPhrase

let getHistory dataAccess session view skip take =
    let account = DataAccess.Account.get dataAccess session

    let outputs = View.Outputs.getAll view dataAccess session

    let getConfirmations confirmationStatus =
        match confirmationStatus with
        | Confirmed (blockNumber,_,blockIndex) ->
            account.blockNumber - blockNumber + 1ul, blockIndex
        | Unconfirmed ->
            0ul,0

    let incoming = List.map (fun output ->
        let txHash = output.outpoint.txHash
        let confirmations,blockIndex = getConfirmations output.confirmationStatus

        txHash, output.spend.asset, output.spend.amount |> bigint,confirmations,blockIndex) outputs

    let outgoing = List.choose (fun output ->
        match output.status with
        | Unspent -> None
        | Spent (txHash,confirmationStatus) ->
            let confirmations,blockIndex = getConfirmations confirmationStatus

            (txHash, output.spend.asset, output.spend.amount |> bigint |> (*) -1I,confirmations,blockIndex)
            |> Some) outputs

    let all =
        incoming @ outgoing
        |> List.fold (fun txs (txHash, asset, amount, confirmations, blockIndex) ->
            match Map.tryFind (txHash, asset) txs with
            | None -> Map.add (txHash, asset) (amount, confirmations, blockIndex) txs
            | Some (amount',_,_) -> Map.add (txHash, asset) (amount + amount', confirmations, blockIndex) txs) Map.empty
        |> Map.toSeq
        |> Seq.map (fun ((txHash, asset),(amount, confirmations, blockIndex)) ->
            if amount >= 0I then
                (txHash,TransactionDirection.In, {asset=asset;amount = uint64 amount}, confirmations, blockIndex)
            else
                (txHash,TransactionDirection.Out, {asset=asset;amount = amount * -1I |> uint64}, confirmations, blockIndex))
        |> List.ofSeq

    let comparer (_,_,_,x1,x2) (_,_,_,y1,y2) =
        if x1 < y1 then
            -1
        elif x1 > y1 then
           1
        else
            y2 - x2

    List.sortWith comparer all
    |> fun xs -> if List.length xs <= skip then [] else List.skip skip xs
    |> List.truncate take
    |> List.map (fun (txHash,direction,spend,confirmations,_) -> txHash,direction,spend,confirmations)
