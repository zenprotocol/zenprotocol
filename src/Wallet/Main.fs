module Wallet.Main

open DataAccess
open Infrastructure
open Messaging.Services
open Messaging.Events
open ServiceBus.Agent
open Consensus
open Types
open Account
open Crypto
open Messaging.Services.Wallet
open Result
open System
open Logary.Message
open Wallet
open Wallet
open Wallet

type AccountStatus =
    | Exist of View.T
    | NoAccount

let eventHandler client event dataAccess session accountStatus =
    match accountStatus with
    | Exist view ->
        match event with
        | TransactionAddedToMemPool (txHash,tx) ->
            let view = View.addMempoolTransaction dataAccess session txHash tx view

            Exist view
        | BlockAdded (blockHash,block) ->
            Account.addBlock dataAccess session blockHash block

            Blockchain.getMempool client
            |> View.fromMempool dataAccess session
            |> Exist

        | BlockRemoved (blockHash,block) ->
            Account.undoBlock dataAccess session blockHash block

            Exist View.empty

        | _ -> accountStatus
    | accountStatus -> accountStatus

let private sync dataAccess session client =
    match Blockchain.getTip client with
    | Some (tipBlockHash,tipHeader) ->
        Account.sync dataAccess session tipBlockHash tipHeader (Blockchain.getBlockHeader client >> Option.get) (Blockchain.getBlock client false >> Option.get)

        eventX "Account synced to block #{blockNumber} {blockHash}"
        >> setField "blockNumber" tipHeader.blockNumber
        >> setField "blockHash" (Hash.toString tipBlockHash)
        |> Log.info

        Blockchain.getMempool client
        |> View.fromMempool dataAccess session
        |> Exist

    | None -> Exist View.empty

let commandHandler client command dataAccess session accountStatus =
    match accountStatus with
    | NoAccount ->
        eventX "Could not handle {command} - no account"
        >> setField "command" (command.ToString())
        |> Log.info

        accountStatus
    | Exist view ->
        match command with
        | Resync ->
            Account.reset dataAccess session
            sync dataAccess session client
        | RestoreNewAddresses maxIndex ->
            Account.restoreNewAddresses dataAccess session maxIndex
            Account.reset dataAccess session
            sync dataAccess session client

let private reply<'a> (requestId:RequestId) (value : Result<'a,string>) =
    requestId.reply value

let publishTx dataAccess session client view publish tx =
    match tx with
    | Ok tx ->
        if publish then
            let ex = Transaction.toExtended tx

            let view = View.addMempoolTransaction dataAccess session ex.txHash tx view
            Blockchain.validateTransaction client ex

            Exist view
        else
            Exist view
    | _ -> Exist view

let requestHandler chain client (requestId:RequestId) request dataAccess session accountStatus =
    match accountStatus with
    | NoAccount ->
        let error = Error "no account"

        match request with
        | AccountExists ->
            Ok false
            |> reply<bool> requestId

            accountStatus
        | ImportSeed (words, password) ->
            let tipHash, tipBlockNumber =
                match Blockchain.getTip client with
                | Some (blockHash,header) -> blockHash, header.blockNumber
                | None -> Hash.zero, 0ul

            match Account.import dataAccess session words password tipHash tipBlockNumber with
            | Ok () ->
                eventX "Account imported"
                |> Log.info

                reply<unit> requestId (Ok ())

                Blockchain.getMempool client
                |> View.fromMempool dataAccess session
                |> Exist

            | Error error ->
                reply<unit> requestId (Error error)
                NoAccount

        | ImportZenPublicKey b58 ->
            match ExtendedKey.fromString chain b58 with
            | Error error ->
                reply<unit> requestId (Error error)
                NoAccount
            | Ok publicKey ->
                let tipHash, tipBlockNumber =
                    match Blockchain.getTip client with
                    | Some (blockHash,header) -> blockHash, header.blockNumber
                    | None -> Hash.zero, 0ul

                match Account.fromZenPublicKey dataAccess session tipHash tipBlockNumber publicKey with
                | Error error ->
                    reply<unit> requestId (Error error)
                    NoAccount
                | Ok () ->
                    eventX "Account imported from zen public key"
                    |> Log.info

                    reply<unit> requestId (Ok ())

                    Blockchain.getMempool client
                    |> View.fromMempool dataAccess session
                    |> Exist
        | GetBalance ->
            error
            |> reply<BalanceResponse> requestId
            NoAccount
        | GetAddressPKHash ->
            error
            |> reply<Hash.Hash> requestId
            NoAccount
        | GetAddress ->
            error
            |> reply<string> requestId
            NoAccount
        | GetTransactionCount _ ->
            error
            |> reply<int> requestId
            NoAccount
        | GetTransactions _ ->
            error
            |> reply<TransactionsResponse> requestId
            NoAccount
        | Send _ ->
            error
            |> reply<Transaction> requestId
            NoAccount
        | Vote _ ->
            error
            |> reply<Transaction> requestId
            NoAccount
        | ActivateContract _ ->
            error
            |> reply<ActivateContractResponse> requestId
            NoAccount
        | ExtendContract _ ->
            error
            |> reply<Transaction> requestId
            NoAccount
        | ExecuteContract _ ->
            error
            |> reply<Transaction> requestId
            NoAccount
        | GetPublicKey _ ->
            error
            |> reply<PublicKey> requestId
            NoAccount
        | Sign _ ->
            error
            |> reply<Crypto.Signature> requestId
            NoAccount
        | CheckPassword _ ->
            error
            |> reply<bool> requestId
            NoAccount
        | GetMnemonicPhrase _ ->
            error
            |> reply<string> requestId
            NoAccount
        | ImportWatchOnlyAddress _ ->
            error
            |> reply<unit> requestId
            NoAccount
        | GetReceivedByAddress _ ->
            error
            |> reply<Map<(string*Asset),uint64>> requestId
            NoAccount
        | GetNewAddress _ ->
            error
            |> reply<string * int> requestId
            NoAccount
        | GetAddressOutputs _ ->
            error
            |> reply<List<(Outpoint*Spend*uint32*bool)>> requestId
            NoAccount
        | GetAddressBalance _ ->
            error
            |> reply<Map<Asset, uint64>> requestId
            NoAccount
        | ExportZenPublicKey ->
            error
            |> reply<string> requestId
            NoAccount
        | RemoveAccount _ ->
            error
            |> reply<unit> requestId
            NoAccount
        | RawTransactionCreate _
        | RawTransactionSign _ ->
            error
            |> reply<RawTransaction> requestId
            NoAccount
        | GetKeys _ ->
            error
            |> reply<Map<PublicKey, string>> requestId
            NoAccount

    | Exist view ->
        let chainParams = Consensus.Chain.getChainParameters chain
        match request with
        | GetBalance ->
            // TODO: add confirmations to request
            Account.getBalance dataAccess session view 0ul
            |> Ok
            |> reply<BalanceResponse> requestId
            accountStatus

        | GetAddressPKHash ->
            Account.getPKHash dataAccess session
            |> Ok
            |> reply<Hash.Hash> requestId

            accountStatus
        | GetAddress ->
            Account.getAddress dataAccess session chain
            |> Ok
            |> reply<string> requestId

            accountStatus
        | GetTransactionCount ->
            Account.getTransactionCount dataAccess session view
            |> Ok
            |> reply<int> requestId
            
            accountStatus
            
        | GetTransactions (skip, take) ->
            Account.getHistory dataAccess session view skip take
            |> Ok
            |> reply<TransactionsResponse> requestId

            accountStatus
        | ImportZenPublicKey _
        | ImportSeed (_, _) ->
            Error "account already exist"
            |> reply<unit> requestId

            accountStatus
        | Send (publish, outputs, password) ->
            match Blockchain.getTip client with
            | Some (_,header) -> 
                let tx =
                    outputs
                    |> List.map (fun (hash, spend) -> { lock = PK hash; spend = spend })
                    |> TransactionCreator.createTransaction chainParams dataAccess session view password header.blockNumber
    
                reply<Transaction> requestId tx
                publishTx dataAccess session client view publish tx
            | None -> 
                Error "No tip" 
                |> reply<Transaction> requestId
                accountStatus
        | Vote (publish, voteData, password) ->
            match Blockchain.getTip client with
            | Some (_,header) -> 
                let allocation, payout =
                    voteData
                    |> (function 
                        | { allocation = allocation ; payout = None} -> 
                           (Some allocation, None)
                        | { allocation = None; payout = payout} -> 
                            (None, Some payout)
                        | _ -> (None,None))
                let tx = TransactionCreator.createVoteTransaction dataAccess session view chainParams header.blockNumber password allocation payout
                reply<Transaction> requestId tx
                publishTx dataAccess session client view publish tx
            | None -> 
                Error "No tip" 
                |> reply<Transaction> requestId
                accountStatus
        | RawTransactionCreate outputs ->
            match Blockchain.getTip client with
            | Some (_,header) -> 
                let raw =
                    outputs
                    |> List.map (fun (hash, spend) -> { lock = PK hash; spend = spend })
                    |> TransactionCreator.createRawTransaction chainParams dataAccess session view None header.blockNumber  
                
                reply<RawTransaction> requestId raw
            | None -> 
                Error "No tip" 
                |> reply<RawTransaction> requestId
            accountStatus
        | RawTransactionSign (raw,password) ->
            let raw = TransactionCreator.signRawTransaction dataAccess session password raw

            reply<RawTransaction> requestId raw
            accountStatus
        | ActivateContract (publish, code, numberOfBlocks, password) ->
            match Blockchain.getTip client with
            | Some (_,header) -> 
                let tx =
                    TransactionCreator.createActivateContractTransaction chainParams dataAccess session view chainParams password code numberOfBlocks header.blockNumber
                    <@> fun tx -> tx, Consensus.Contract.makeContractId Version0 code
                reply<ActivateContractResponse> requestId tx
                publishTx dataAccess session client view publish (Result.map fst tx)
            | None -> 
                Error "No tip" 
                |> reply<ActivateContractResponse> requestId
                accountStatus
        | ExtendContract (publish, contractId, numberOfBlocks, password) ->
            match Blockchain.getTip client with
            | Some (_,header) -> 
                let tx = TransactionCreator.createExtendContractTransaction dataAccess session view (Blockchain.getActiveContract client) chainParams password contractId numberOfBlocks header.blockNumber

                reply<Transaction> requestId tx
                publishTx dataAccess session client view publish tx
            | None -> 
                Error "No tip" 
                |> reply<ActivateContractResponse> requestId
                accountStatus

        | ExecuteContract (publish, contractId, command, data, provideReturnAddress, sign, spends, password) ->
            match Blockchain.getTip client with
            | Some (_,header) -> 
                let tx = TransactionCreator.createExecuteContractTransaction chainParams dataAccess session view (Blockchain.executeContract client) password contractId command data provideReturnAddress sign spends header.blockNumber
                reply<Transaction> requestId tx
                publishTx dataAccess session client view publish tx
            | None -> 
                Error "No tip" 
                |> reply<Transaction> requestId
                accountStatus
        | AccountExists ->
            Ok true
            |> reply<bool> requestId

            accountStatus
        | GetPublicKey (path, password) ->
            Account.getPublicKey dataAccess session password path
            |> reply<PublicKey> requestId

            accountStatus
        | Sign (message, path, password) ->
            Account.sign dataAccess session password path message
            |> reply<Crypto.Signature> requestId

            accountStatus
        | CheckPassword password ->
            match Account.checkPassword dataAccess session password with
            | Ok _ -> Ok true
            | Error error when error = Security.BadPassword -> Ok false
            | Error error -> Error error
            |> reply<bool> requestId

            accountStatus
        | GetMnemonicPhrase password ->
           Account.getMnemonicPhrase dataAccess session password
           |> reply<string> requestId

           accountStatus

        | ImportWatchOnlyAddress address ->
            Account.importWatchOnlyAddress dataAccess session chain address
            |> reply<unit> requestId

            accountStatus
        | GetNewAddress ->
            Account.getNewAddress dataAccess session chain
            |> reply<string * int> requestId

            accountStatus
        | GetReceivedByAddress confirmations ->
            Account.getReceived dataAccess session view chain confirmations
            |> Ok
            |> reply<Map<(string*Asset),uint64>> requestId

            accountStatus

        | GetAddressOutputs address ->
            Account.getAddressOutputs dataAccess session view chain address
            |> reply<List<(Outpoint*Spend*uint32*bool)>> requestId

            accountStatus
        | GetAddressBalance (address,confirmations) ->
            Account.getAddressBalance dataAccess session view chain address confirmations
            |> reply<Map<Asset, uint64>> requestId

            accountStatus
        | ExportZenPublicKey ->
            Account.getZenExtendedPublicKey dataAccess session
            |> ExtendedKey.toString chain
            |> Ok
            |> reply<string> requestId

            accountStatus
        | RemoveAccount password ->
            match Account.checkPassword dataAccess session password with
            | Ok _ ->
                Account.delete dataAccess session

                Ok ()
                |> reply<unit> requestId

                NoAccount
            | Error error ->
                Error error
                |> reply<unit> requestId

                accountStatus
        | GetKeys password ->
            Account.getKeys dataAccess session password
            |> reply<Map<PublicKey, string>> requestId

            accountStatus

type Wipe =
    | Full
    | Reset
    | NoWipe

let main dataPath busName chain (wipe:Wipe) =
    let dataPath = Platform.combine dataPath "walletdb"

    if wipe = Full then
        eventX "Wiping wallet database"
        |> Log.info
        if System.IO.Directory.Exists dataPath then
                System.IO.Directory.Delete (dataPath,true)

    Actor.create<Command,Request,Event, AccountStatus> busName serviceName (fun _ sbObservable ebObservable ->
        let databaseContext = DataAccess.createContext dataPath
        let dataAccess = DataAccess.init databaseContext

        let client = ServiceBus.Client.create busName
        let accountStatus =
            use session = DatabaseContext.createSession databaseContext

            let account = DataAccess.Account.tryGet dataAccess session

            let accountStatus =
                match wipe, account with
                | Reset, Some _  ->
                    eventX "Resetting account"
                    |> Log.info

                    Account.reset dataAccess session
                    sync dataAccess session client
                | NoWipe, Some _ ->
                    eventX "Syncing account..."
                    |> Log.info

                    sync dataAccess session client
                | _, _ -> NoAccount

            Session.commit session

            accountStatus

        let sbObservable =
            sbObservable
            |> Observable.map (fun message ->
                match message with
                | ServiceBus.Agent.Command c -> commandHandler client c
                | ServiceBus.Agent.Request (requestId, r) -> requestHandler chain client requestId r)

        let ebObservable =
            ebObservable
            |> Observable.map (eventHandler client)

        let observable =
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun account handler ->
                use session = DatabaseContext.createSession databaseContext
                let accountStatus = handler dataAccess session account

                Session.commit session
                accountStatus) accountStatus

        Disposables.fromFunction (fun () ->
            DataAccess.dispose dataAccess
            Disposables.dispose databaseContext), observable
    )
