module Wallet.Main

open FsNetMQ
open Infrastructure
open Messaging.Services
open Messaging.Events
open Wallet
open ServiceBus.Agent
open DataAccess
open MBrace.FsPickler

type AccountCollection = Collection<string, Account.T>

[<Literal>]
let MainAccountName = "MAIN"


let binarySerializer = FsPickler.CreateBinarySerializer()

let eventHandler collection event session account =
    match account, event with
    | Some account, TransactionAddedToMemPool (txHash,tx) ->
        let account = Account.addTransaction txHash tx account
        Collection.put collection session MainAccountName account
        Some account
    | Some account, BlockAdded (blockHash,block) ->
        let account = Account.handleBlock blockHash block account
        Collection.put collection session MainAccountName account
        Some account
    | Some account, BlockRemoved (_,block) ->
        let account = Account.undoBlock block account
        Collection.put collection session MainAccountName account
        Some account
    | _ -> account

let commandHandler command session wallet = wallet

let private sync (account:Account.T) chainParams client =
    match Blockchain.getTip client with
    | Some (blockHash,header) ->
        if blockHash <> account.tip then
            let account =
                Account.sync chainParams blockHash
                    (Blockchain.getBlockHeader client >> Option.get)
                    (Blockchain.getBlock client >> Option.get)
                    account

            Log.info "Account synced to block #%d %A" header.blockNumber blockHash
            account
        else
            account
    | _ -> account

let requestHandler collection chain client (requestId:RequestId) request session wallet =
    let chainParams = Consensus.Chain.getChainParameters chain
    let reply =
        requestId.reply
    let getTransactionResult =
        function
        | Result.Ok tx ->
            TransactionResult.Ok tx
        | Result.Error err ->
            TransactionResult.Error err

    match wallet, request with
    | Some wallet, GetBalance ->
        Account.getBalance wallet
        |> reply
        Some wallet
    | Some wallet, GetAddressPKHash ->
        reply wallet.publicKeyHash
        Some wallet
    | Some wallet, GetAddress ->
        Account.getAddress chain wallet
        |> reply
        Some wallet
    | Some wallet, GetTransactions ->
        Account.getHistory wallet
        |> reply
        Some wallet
    | _, ImportSeed words ->
        match Account.import words "" with 
        | Result.Ok account -> 
            Log.info "Account imported"
            ImportResult.Ok ()
            |> reply
            let account = sync account chainParams client
            Collection.put collection session MainAccountName account
            Some account
        | Result.Error err -> 
            ImportResult.Error err
            |> reply
            wallet
    | Some wallet, Spend (address, spend) ->
        Account.createTransaction wallet address spend
        |> getTransactionResult
        |> reply
        Some wallet
    | Some wallet, ActivateContract (code,numberOfBlocks) ->
        Account.createActivateContractTransaction chainParams wallet code numberOfBlocks
        |> function     // TODO: cleanup
        | Result.Ok tx ->
            ActivateContractTransactionResult.Ok (tx, Consensus.Contract.computeHash code)
        | Result.Error err ->
            ActivateContractTransactionResult.Error err
        |> reply
        Some wallet
    | Some wallet, ExecuteContract (cHash,command,data,spends) ->
        let executeContract = Blockchain.executeContract client
        Account.createExecuteContractTransaction wallet executeContract cHash command data spends
        |> getTransactionResult
        |> reply
        Some wallet
    | wallet, AccountExists ->
        reply (Option.isSome wallet)
        wallet
    | _ ->
        failwithf "Could not handle %A " request

let main dataPath busName chain root =
    let chainParams = Consensus.Chain.getChainParameters chain
    Actor.create<Command,Request,Event, Account.T Option> busName serviceName (fun poller sbObservable ebObservable ->
        let databaseContext = DatabaseContext.create (Platform.combine dataPath "wallet")

        let collection : AccountCollection =
            use session = DatabaseContext.createSession databaseContext
            let collection = Collection.create session "accounts"
                                (fun (name:string) -> System.Text.Encoding.UTF8.GetBytes (name))
                                binarySerializer.Pickle<Account.T>
                                binarySerializer.UnPickle<Account.T>
            Session.commit session
            collection

        let client = ServiceBus.Client.create busName
        let account =
            use session = DatabaseContext.createSession databaseContext

            match Collection.tryGet collection session MainAccountName with
            | Some account ->
                sync account chainParams client
                |> Some
            | None ->
                None

        let sbObservable =
            sbObservable
            |> Observable.map (fun message ->
                match message with
                | ServiceBus.Agent.Command c -> commandHandler c
                | ServiceBus.Agent.Request (requestId, r) -> requestHandler collection chain client requestId r)

        let ebObservable =
            ebObservable
            |> Observable.map (eventHandler collection)

        let observable =
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun state handler ->
                use session = DatabaseContext.createSession databaseContext
                let state = handler session state
                Session.commit session
                state) account

        Disposables.fromFunction (fun () ->
            Disposables.dispose collection
            Disposables.dispose databaseContext), observable
    )
