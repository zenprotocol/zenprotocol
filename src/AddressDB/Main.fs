module AddressDB.Main

open DataAccess
open Infrastructure
open Messaging.Services
open Messaging.Services.Wallet
open Messaging.Events
open ServiceBus.Agent
open Consensus
open Types
open Crypto
open Messaging.Services.AddressDB
open Result
open Logary.Message

let eventHandler client event dataAccess session view =
    match event with
    | TransactionAddedToMemPool (txHash,tx) ->
        View.addMempoolTransaction dataAccess session txHash tx view
    | BlockAdded (blockHash,block) ->
        Repository.addBlock dataAccess session blockHash block

        Blockchain.getMempool client
        |> View.fromMempool dataAccess session
    | BlockRemoved (blockHash,block) ->
        Repository.undoBlock dataAccess session blockHash block
        View.empty
    | _ ->
        view

let rec private sync dataAccess session client =
    match Blockchain.getTip client with
    | Some (tipBlockHash,tipHeader) ->
        Repository.sync dataAccess session tipBlockHash tipHeader (Blockchain.getBlockHeader client >> Option.get) (Blockchain.getBlock client false >> Option.get)
        
        match Blockchain.getTip client with
        | Some (currentTipBlockHash,_) when currentTipBlockHash = tipBlockHash ->
            eventX "AddressDB synced to block #{blockNumber} {blockHash}"
            >> setField "blockNumber" tipHeader.blockNumber
            >> setField "blockHash" (Hash.toString tipBlockHash)
            |> Log.info
            
            Blockchain.getMempool client
            |> View.fromMempool dataAccess session
        | _ -> // tip has changed while syncing, sync again
            sync dataAccess session client
    | None -> View.empty

let commandHandler client command dataAccess session view =
    match command with
    | Resync ->
        Repository.reset dataAccess session
        sync dataAccess session client

let private reply<'a> (requestId:RequestId) (value : Result<'a,string>) =
    requestId.reply value

let requestHandler chain (requestId:RequestId) request dataAccess session view =
    let decodeAddresses = Result.traverseResultM (Wallet.Address.decodeAny chain)
    match request with
    | GetBalance addresses ->
        decodeAddresses addresses
        <@> Repository.getBalance dataAccess session view UnspentOnly
        |> reply<BalanceResponse> requestId
    | GetOutputs (addresses, mode) ->
        decodeAddresses addresses
        <@> Repository.getOutputs dataAccess session view mode
        |> reply<List<PointedOutput>> requestId
    | GetTransactionCount (addresses, blockNumber) ->
        decodeAddresses addresses
        <@> Repository.getTransactionCount dataAccess session view blockNumber
        |> reply<int> requestId
    | GetTransactions (addresses, skip, take) ->
        decodeAddresses addresses
        <@> Repository.getHistory dataAccess session view skip take
        |> reply<TransactionsResponse> requestId
    | GetContractHistory (contractId, skip, take) ->
        Repository.getContractHistory dataAccess session view skip take contractId
        |> Ok
        |> reply<ContractHistoryResponse> requestId
    |> ignore
    
    view

type Wipe =
    | Full
    | Reset
    | NoWipe

let main dataPath busName chain (wipe:Wipe) =
    let dataPath = Platform.combine dataPath "addressdb"

    if wipe = Full then
        eventX "Wiping AddressDB database"
        |> Log.info
        
        if System.IO.Directory.Exists dataPath then
            System.IO.Directory.Delete (dataPath,true)

    Actor.create<Command,Request,Event, View.T> busName serviceName (fun _ sbObservable ebObservable ->
        let databaseContext = DataAccess.createContext dataPath
        let dataAccess = DataAccess.init databaseContext

        let client = ServiceBus.Client.create busName
        use session = DatabaseContext.createSession databaseContext

        match DataAccess.Tip.tryGet dataAccess session with 
        | None -> 
            eventX "Creating AddressDB"
            |> Log.info
            
            Repository.init dataAccess session
        | Some _ -> ()
            
        if wipe = Reset then
            eventX "Resetting AddressDB"
            |> Log.info
            
            Repository.reset dataAccess session
                
        let view = sync dataAccess session client
            
        Session.commit session

        let sbObservable =
            sbObservable
            |> Observable.map (fun message ->
                match message with
                | ServiceBus.Agent.Request (requestId, r) -> requestHandler chain requestId r
                | ServiceBus.Agent.Command command -> commandHandler client command
            )
                
        let ebObservable =
            ebObservable
            |> Observable.map (eventHandler client)

        let observable =
            Observable.merge sbObservable ebObservable
            |> Observable.scan (fun view handler ->
                use session = DatabaseContext.createSession databaseContext
                let view = handler dataAccess session view

                Session.commit session
                
                view
            ) view

        Disposables.fromFunction (fun () ->
            DataAccess.dispose dataAccess
            Disposables.dispose databaseContext), observable
    )
