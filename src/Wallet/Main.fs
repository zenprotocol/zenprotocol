module Wallet.Main

open FsNetMQ
open Infrastructure
open Messaging.Services
open Messaging.Events
open Wallet
open ServiceBus.Agent
open DataAccess
open MBrace.FsPickler
open Consensus
open Account
open Messaging.Services.Wallet

let private (<@>) a b = Result.map b a
let private (>>=) a b = Result.bind b a

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

let checkWallet =
    function
    | Some wallet -> Ok wallet
    | _ -> Error "no wallet"

let commandHandler chain client command session account =
    let chainParams = Consensus.Chain.getChainParameters chain
    let checkWallet = checkWallet account
    
    match command with
    | Resync ->
        checkWallet
        <@> fun account ->
                let account = { account with deltas = List.empty; outputs=Map.empty; tip = Hash.zero; blockNumber = 0ul }
                sync account chainParams client
    |> function 
    | Ok account ->
        Some account
    | Error error -> 
        Log.info "Could not handle command due to %A " error
        account

let private reply<'a> (requestId:RequestId) (value : Result<'a,string>) =
    requestId.reply value

let requestHandler collection chain client (requestId:RequestId) request session wallet =
    let chainParams = Consensus.Chain.getChainParameters chain
    let checkWallet = checkWallet wallet

    match request with
    | GetBalance ->
        checkWallet
        <@> Account.getBalance
        |> reply<BalanceResponse> requestId
        wallet
    | GetAddressPKHash ->
        checkWallet
        <@> fun wallet -> wallet.publicKeyHash
        |> reply<Hash.Hash> requestId
        wallet
    | GetAddress ->
        checkWallet
        <@> Account.getAddress chain
        |> reply<string> requestId
        wallet
    | GetTransactions ->
        checkWallet
        <@> Account.getHistory
        |> reply<TransactionsResponse> requestId
        wallet
    | ImportSeed words ->
        Account.import words ""
        <@> fun account -> 
                Collection.put collection session MainAccountName account
                Log.info "Account imported"
                account
        |> function
        | Ok account -> 
            reply<unit> requestId (Ok ())
            Some account
        | Error error ->
            reply<unit> requestId (Error error)
            wallet
    | Spend (address, spend) ->
        checkWallet
        >>= fun wallet -> Account.createTransaction wallet address spend
        |> reply<Types.Transaction> requestId
        wallet
    | ActivateContract (code,numberOfBlocks) ->
        checkWallet
        >>= fun wallet -> Account.createActivateContractTransaction chainParams wallet code numberOfBlocks
        <@> fun tx -> tx, Consensus.Contract.computeHash code
        |> reply<ActivateContractResponse> requestId
        wallet
    | ExecuteContract (cHash,command,data,spends) ->
        checkWallet
        >>= fun wallet -> Account.createExecuteContractTransaction wallet (Blockchain.executeContract client) cHash command data spends
        |> reply<Types.Transaction> requestId
        wallet
    | AccountExists ->
        wallet
        |> Option.isSome
        |> Ok
        |> reply<bool> requestId
        wallet
    | _ ->
        Log.info "Could not handle %A " request
        wallet

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
                | ServiceBus.Agent.Command c -> commandHandler chain client c
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
