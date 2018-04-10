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

type AccountData = Account.T Option * ExtendedKey.T Option

let checkWallet =
    function
    | Some wallet -> Ok (wallet)
    | _ -> Error "no wallet"

let eventHandler event _ _ (account, extendedKey) =
    let checkWallet = checkWallet account

    (match event with
    | TransactionAddedToMemPool (txHash,tx) ->
        checkWallet
        <@> Account.addTransaction txHash tx
    | BlockAdded (blockHash,block) ->
        checkWallet
        <@> Account.handleBlock blockHash block
    | BlockRemoved (_,block) ->
        checkWallet
        <@> Account.undoBlock block
    | _ -> Error ""
    |> function
    | Ok account -> Some account
    | Error error ->
        if error <> "" then
            eventX "Could not handle event {event} due to {error}"
            >> setField "event" (event.GetType().Name)
            >> setField "error" error
            |> Log.info

        account), extendedKey

let private sync chainParams client account =
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

let checkWalletSecured wallet extendedKey =
    checkWallet wallet
    >>= fun wallet ->
        match extendedKey with
        | Some extendedKey -> Ok (wallet, extendedKey)
        | None -> Error "wallet locked"

let commandHandler chain client command _ _ (account, extendedKey) =
    let chainParams = Consensus.Chain.getChainParameters chain
    let checkWallet = checkWallet account
    (match command with
    | Resync ->
        let resetAccount account = { account with deltas = List.empty; outputs=Map.empty; tip = Hash.zero; blockNumber = 0ul }
        checkWallet
        <@> (resetAccount >> sync chainParams client)
        <@> fun account -> Some account, extendedKey
    | Lock->
        eventX "Account locked"
        |> Log.info

        Ok (account, None)
    |> function
    | Ok ret -> ret
    | Error error ->
        if error <> "" then
            eventX "Could not handle command {command} due to {error}"
            >> setField "command" (command.GetType().Name)
            >> setField "error" error
            |> Log.info

        account, extendedKey)

let private reply<'a> (requestId:RequestId) (value : Result<'a,string>) =
    requestId.reply value

let requestHandler chain client (requestId:RequestId) request dataAccess session (wallet, extendedKey) =
    let chainParams = Consensus.Chain.getChainParameters chain
    let checkWallet = checkWallet wallet
    let checkWalletSecured = checkWalletSecured wallet extendedKey

    match request with
    | GetBalance ->
        checkWallet
        <@> Account.getBalance
        |> reply<BalanceResponse> requestId
        wallet, extendedKey
    | GetAddressPKHash ->
        checkWallet
        <@> fun wallet -> wallet.publicKey
                          |> PublicKey.hash
        |> reply<Hash.Hash> requestId
        wallet, extendedKey
    | GetAddress ->
        checkWallet
        <@> fun wallet -> wallet.publicKey
                          |> PublicKey.hash
                          |> Address.PK
                          |> Address.encode chain
        |> reply<string> requestId
        wallet, extendedKey
    | GetTransactions ->
        checkWallet
        <@> Account.getHistory
        |> reply<TransactionsResponse> requestId
        wallet, extendedKey
    | ImportSeed (words, password) ->
        Account.import words password
        <@> fun (account, secured) ->
                DataAccess.Account.put dataAccess session account
                DataAccess.Secured.put dataAccess session secured
                eventX "Account imported and secured"
                |> Log.info
                account
        |> function
        | Ok account ->
            reply<unit> requestId (Ok ())
            Some account, extendedKey
        | Error error ->
            reply<unit> requestId (Error error)
            wallet, extendedKey
    | Spend (address, spend) ->
        checkWalletSecured
        >>= Account.createTransaction address spend
        |> reply<Types.Transaction> requestId
        wallet, extendedKey
    | ActivateContract (code,numberOfBlocks) ->
        checkWalletSecured
        >>= Account.createActivateContractTransaction chainParams code numberOfBlocks
        <@> fun tx -> tx, Consensus.Contract.computeHash code
        |> reply<ActivateContractResponse> requestId
        wallet, extendedKey
    | ExecuteContract (cHash,command,data,provideReturnAddress, sign, spends) ->
        checkWalletSecured
        >>= Account.createExecuteContractTransaction (Blockchain.executeContract client) cHash command data provideReturnAddress sign spends
        |> reply<Types.Transaction> requestId
        wallet, extendedKey
    | AccountExists ->
        wallet
        |> Option.isSome
        |> Ok
        |> reply<bool> requestId
        wallet, extendedKey
    | AccountLocked ->
        extendedKey
        |> Option.isNone
        |> Ok
        |> reply<bool> requestId
        wallet, extendedKey
    | GetPublicKey path ->
        try
            match extendedKey with
            | None ->
                reply<PublicKey> requestId (Error "wallet locked")
            | Some extendedKey ->
                ExtendedKey.derivePath path extendedKey
                >>= ExtendedKey.getPublicKey
                |> reply<PublicKey> requestId
        with
        | x -> printfn "%A" x
        wallet,extendedKey
    | Unlock password ->
        wallet,
        match DataAccess.Secured.tryGet dataAccess session with
        | Some secured ->
            match Secured.decrypt password secured with
            | Ok extendedKey ->
                eventX "Account unlocked"
                |> Log.info
                reply<unit> requestId (Ok ())
                Some extendedKey
            | Error error ->
                reply<unit> requestId (Error <| sprintf "Could not unlock wallet: %A" error)
                None
        | None ->
            reply<unit> requestId (Error "Could not unlock wallet - no data")
            None

let main dataPath busName chain =
    let chainParams = Consensus.Chain.getChainParameters chain
    Actor.create<Command,Request,Event, AccountData> busName serviceName (fun poller sbObservable ebObservable ->
        let databaseContext = DataAccess.createContext dataPath
        let dataAccess = DataAccess.init databaseContext

        let client = ServiceBus.Client.create busName
        let account =
            use session = DatabaseContext.createSession databaseContext

            DataAccess.Account.tryGet dataAccess session
            |> Option.map (fun account ->
                eventX "Account found. syncing"
                |> Log.info
                sync chainParams client account)

        let sbObservable =
            sbObservable
            |> Observable.map (fun message ->
                match message with
                | ServiceBus.Agent.Command c -> commandHandler chain client c
                | ServiceBus.Agent.Request (requestId, r) -> requestHandler chain client requestId r)

        let ebObservable =
            ebObservable
            |> Observable.map eventHandler

        let observable =
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun (wallet, key) handler ->
                use session = DatabaseContext.createSession databaseContext
                let wallet = handler dataAccess session (wallet, key)

                Session.commit session
                wallet) (account, None)

        Disposables.fromFunction (fun () ->
            DataAccess.dispose dataAccess
            Disposables.dispose databaseContext), observable
    )
