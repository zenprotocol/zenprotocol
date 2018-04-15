module Wallet.Main

open DataAccess
open Infrastructure
open Messaging.Services
open Messaging.Events
open ServiceBus.Agent
open Consensus
open Account
open Consensus.Crypto
open Messaging.Services.Wallet
open Result
open System
open Logary.Message

let eventHandler event _ _ account =
    account
    |> Option.map (
        match event with
        | TransactionAddedToMemPool (txHash,tx) ->
            Account.addTransaction txHash tx
        | BlockAdded (blockHash,block) ->
            Account.handleBlock blockHash block
        | BlockRemoved (_,block) ->
            Account.undoBlock block
        | _ -> id)

let private sync client account =
    match Blockchain.getTip client with
    | Some (blockHash,header) when blockHash <> account.tip ->
        account
        |> Account.sync blockHash
            (Blockchain.getBlockHeader client >> Option.get)
            (Blockchain.getBlock client >> Option.get)
        |> fun account ->
            eventX "Account synced to block #{blockNumber} {blockHash}"
            >> setField "blockNumber" header.blockNumber
            >> setField "blockHash" (Hash.toString blockHash)
            |> Log.info
            account
    | _ -> account


let commandHandler client command _ _ account =
    match account with
    | None ->
        eventX "Could not handle {command} - no account"
        >> setField "command" (command.ToString())
        |> Log.info
        account
    | Some account ->
        match command with
        | Resync -> 
            { account with deltas = List.empty; outputs=Map.empty; tip = Hash.zero; blockNumber = 0ul }
            |> sync client
            |> Some
            
let private reply<'a> (requestId:RequestId) (value : Result<'a,string>) =
    requestId.reply value

let requestHandler chain client (requestId:RequestId) request dataAccess session account =
    let chainParams = Consensus.Chain.getChainParameters chain
    let getAccount =
        match account with
        | Some account -> Ok account
        | _ -> Error "No account"
    let unlockAccount password =
        getAccount
        >>= fun account ->
            match DataAccess.Secured.tryGet dataAccess session with
            | Some secured ->
                Secured.decrypt password secured
                <@> fun extendedKey -> account, extendedKey
            | None ->
                Error "Could not unlock account - no data"

    match request with
    | GetBalance ->
        getAccount
        <@> Account.getBalance
        |> reply<BalanceResponse> requestId
        account
    | GetAddressPKHash ->
        getAccount
        <@> fun account -> account.publicKey
                          |> PublicKey.hash
        |> reply<Hash.Hash> requestId
        account
    | GetAddress ->
        getAccount
        <@> fun account -> account.publicKey
                          |> PublicKey.hash
                          |> Address.PK
                          |> Address.encode chain
        |> reply<string> requestId
        account
    | GetTransactions ->
        getAccount
        <@> Account.getHistory
        |> reply<TransactionsResponse> requestId
        account
    | ImportSeed (words, password) ->
        Account.import words password
        <@> fun (account, secured) ->
                DataAccess.Account.put dataAccess session account
                DataAccess.Secured.put dataAccess session secured
                eventX "Account imported"
                |> Log.info
                account
        |> function
        | Ok account ->
            reply<unit> requestId (Ok ())
            Some account
        | Error error ->
            reply<unit> requestId (Error error)
            account
    | Send (address, spend, password) ->
        unlockAccount password
        >>= Account.createTransaction address spend
        |> reply<Types.Transaction> requestId
        account
    | ActivateContract (code, numberOfBlocks, password) ->
        unlockAccount password
        >>= Account.createActivateContractTransaction chainParams code numberOfBlocks
        <@> fun tx -> tx, Consensus.Contract.computeHash code
        |> reply<ActivateContractResponse> requestId
        account
    | ExecuteContract (cHash, command, data, provideReturnAddress, sign, spends, password) ->
        unlockAccount password
        >>= Account.createExecuteContractTransaction (Blockchain.executeContract client) cHash command data provideReturnAddress sign spends
        |> reply<Types.Transaction> requestId
        account
    | AccountExists ->
        account
        |> Option.isSome
        |> Ok
        |> reply<bool> requestId
        account
    | GetPublicKey (path, password) ->
        unlockAccount password
        >>= (snd >> ExtendedKey.derivePath path)
        >>= ExtendedKey.getPublicKey
        |> reply<PublicKey> requestId
        account
    | CheckPassword password ->
        match unlockAccount password with
        | Ok _ -> Ok true
        | Error error when error = Security.BadPassword -> Ok false 
        | Error error -> Error error
        |> reply<bool> requestId
        account

let main dataPath busName chain =
    Actor.create<Command,Request,Event, Option<Account.T>> busName serviceName (fun poller sbObservable ebObservable ->
        let databaseContext = DataAccess.createContext dataPath
        let dataAccess = DataAccess.init databaseContext

        let client = ServiceBus.Client.create busName
        let account =
            use session = DatabaseContext.createSession databaseContext

            DataAccess.Account.tryGet dataAccess session
            |> Option.map (fun account ->
                eventX "Account exists. syncing..."
                |> Log.info
                sync client account)

        let sbObservable =
            sbObservable
            |> Observable.map (fun message ->
                match message with
                | ServiceBus.Agent.Command c -> commandHandler client c
                | ServiceBus.Agent.Request (requestId, r) -> requestHandler chain client requestId r)

        let ebObservable =
            ebObservable
            |> Observable.map eventHandler

        let observable =
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun account handler ->
                use session = DatabaseContext.createSession databaseContext
                let account = handler dataAccess session account

                Session.commit session
                account) account

        Disposables.fromFunction (fun () ->
            DataAccess.dispose dataAccess
            Disposables.dispose databaseContext), observable
    )
