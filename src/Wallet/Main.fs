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
    match event with
    | TransactionAddedToMemPool (txHash,tx) ->
        let account = Account.addTransaction txHash tx account
        Collection.put collection session MainAccountName account
        account
    | BlockAdded (blockHash,block) ->
        let account = Account.handleBlock blockHash block account
        Collection.put collection session MainAccountName account
        account
    | BlockRemoved (_,block) ->
        let account = Account.undoBlock block account
        Collection.put collection session MainAccountName account
        account
    | _ -> account

let commandHandler command session wallet = wallet

let requestHandler chain client (requestId:RequestId) request session wallet =
    let reply =
        requestId.reply
    let getTransactionResult =
        function
        | Result.Ok tx ->
            TransactionResult.Ok tx
        | Result.Error err ->
            TransactionResult.Error err

    match request with
    | GetBalance ->
        Account.getBalance wallet
        |> reply
    | GetAddressPKHash ->
        reply wallet.publicKeyHash
    | GetAddress ->
        Account.getAddress chain wallet
        |> reply
    | Spend (address, spend) ->
        Account.createTransaction wallet address spend
        |> getTransactionResult
        |> reply
    | ActivateContract (code,numberOfBlocks) ->
        Account.createActivateContractTransaction chain wallet code numberOfBlocks
        |> function     // TODO: cleanup
        | Result.Ok tx ->
            ActivateContractTransactionResult.Ok (tx, Consensus.Contract.computeHash code)
        | Result.Error err ->
            ActivateContractTransactionResult.Error err
        |> reply
    | ExecuteContract (cHash,command,data,spends) ->
        let executeContract = Blockchain.executeContract client
        Account.createExecuteContractTransaction wallet executeContract cHash command data spends
        |> getTransactionResult
        |> reply

    wallet

let main dataPath busName chain root =
    Actor.create<Command,Request,Event, Account.T> busName serviceName (fun poller sbObservable ebObservable ->
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
            let account =
                match Collection.tryGet collection session MainAccountName with
                | Some account ->
                    Log.info "Account Loaded"
                    account
                | None ->
                    Log.info "Creating new account"
                    if root then
                        Account.rootAccount
                    else
                        Account.create ()

            match Blockchain.getTip client with
            | Some (blockHash,header) ->

                if blockHash <> account.tip then
                    let account =
                        Account.sync chain blockHash
                            (Blockchain.getBlockHeader client >> Option.get)
                            (Blockchain.getBlock client >> Option.get)
                            account

                    Collection.put collection session MainAccountName account
                    Session.commit session

                    Log.info "Account synced to block #%d %A" header.blockNumber blockHash
                    account
                else
                    account

            | _ -> account

        let sbObservable =
            sbObservable
            |> Observable.map (fun message ->
                match message with
                | ServiceBus.Agent.Command c -> commandHandler c
                | ServiceBus.Agent.Request (requestId, r) -> requestHandler chain client requestId r)

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
