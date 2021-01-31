module AddressDB.Main

open Blockchain
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

type ActorStatus =
    | Running of View.T
    | Stop

let eventHandler client event dataAccess session status =
    match status with
    | Running view ->
        match event with
        | TransactionAddedToMemPool (txHash,tx) ->
            View.addMempoolTransaction dataAccess session txHash tx view
        | BlockAdded (_,block) ->
            Repository.addBlock dataAccess session block

            Blockchain.getMempool client
            |> View.fromMempool dataAccess session
        | BlockRemoved (_,block) ->
            Repository.undoBlock dataAccess session block
            View.empty
        | _ ->
            view
        |> Running
    | status -> status

let private sync dataAccess session client =
    match Blockchain.getTip client with
    | Some (tipBlockHash, tipHeader) ->
        let account = 
            DataAccess.Tip.tryGet dataAccess session
            |> Option.map (fun x -> x.blockNumber)
            |> Option.defaultValue 0ul

        if tipHeader.blockNumber <> account then 
            Blockchain.getAllBlocks client (int account)
            |> Map.map (fun _ b -> Serialization.Block.deserialize b |> Option.get) //this is sent over the messaging bus so we can be sure about the existences
            |> Repository.sync dataAccess session tipBlockHash tipHeader


        eventX "AddressDB synced to block #{blockNumber} {blockHash}"
        >> setField "blockNumber" tipHeader.blockNumber
        >> setField "blockHash" (Hash.toString tipBlockHash)
        |> Log.info

        Blockchain.getMempool client
        |> View.fromMempool dataAccess session
    | None ->
        View.empty
    |> Running

let commandHandler client command dataAccess session status =
    match status with
    | Stop ->
        eventX "Could not handle {command} - not running"
        >> setField "command" (command.ToString())
        |> Log.info

        status
    | Running _ ->
        match command with
        | Resync ->
            Repository.reset dataAccess session
            sync dataAccess session client

let private reply<'a> (requestId:RequestId) (value : Result<'a,string>) =
    requestId.reply value

let requestHandler chain (requestId:RequestId) request dataAccess session (status: ActorStatus) =
    match status with
    | Stop ->
        let error = Error "AddressDB not running"
        match request with
        | GetBalance _ ->
            error
            |> reply<BalanceResponse> requestId
        | GetOutputs _ ->
            error
            |> reply<List<PointedOutput>> requestId
        | GetTransactionCount _ ->
            error      
            |> reply<int> requestId
        | GetTransactions _ ->
            error
            |> reply<TransactionsResponse> requestId
        | GetContractHistory _ ->
            error
            |> reply<ContractHistoryResponse> requestId
        | GetContractAssets _ ->
            error
            |> reply<option<uint32 * string option * string * Zen.Types.Data.data option>> requestId
        | GetContractInfo _ -> 
            error
            |> reply<ContractId * ContractV0> requestId
        status
    | Running view ->
        let decodeAddresses = Result.traverseResultM (Wallet.Address.decodeAny chain)
        match request with
        | GetBalance (addresses, blockNumber) ->
            decodeAddresses addresses
            <@> View.getBalance dataAccess session view UnspentOnly blockNumber
            |> reply<BalanceResponse> requestId
        | GetOutputs (addresses, mode, blockNumber) ->
            decodeAddresses addresses
            <@> View.getOutputs dataAccess session view mode blockNumber
            |> reply<List<PointedOutput>> requestId
        | GetTransactionCount (addresses, blockNumber) ->
            decodeAddresses addresses
            <@> View.getTransactionCount dataAccess session view blockNumber
            |> reply<int> requestId
        | GetTransactions (addresses, skip, take) ->
            decodeAddresses addresses
            <@> View.getHistory dataAccess session view skip take
            |> reply<TransactionsResponse> requestId
        | GetContractHistory (contractId, skip, take) ->
            View.getContractHistory dataAccess session view skip take contractId
            |> Ok
            |> reply<ContractHistoryResponse> requestId
        | GetContractAssets asset ->
            View.getContractAsset dataAccess session view asset
            |> Ok
            |> reply<option<uint32 * string option * string * Zen.Types.Data.data option>> requestId
        | GetContractInfo (code, rlimit) ->
            View.getContractInfo rlimit code
            |> reply<ContractId * ContractV0> requestId
        
        status

type Wipe =
    | Full
    | Reset
    | NoWipe

let main dataPath busName chain (isRunning:bool) (wipe:Wipe) =
    let dataPath = Platform.combine dataPath "addressdb"

    if wipe = Full then
        eventX "Wiping AddressDB database"
        |> Log.info

        if System.IO.Directory.Exists dataPath then
            System.IO.Directory.Delete (dataPath,true)

    Actor.create<Command,Request,Event, ActorStatus> busName serviceName (fun _ sbObservable ebObservable ->
        let databaseContext = DataAccess.createContext dataPath
        let dataAccess = DataAccess.init databaseContext

        let client = ServiceBus.Client.create busName
        let status =
            use session = DatabaseContext.createSession databaseContext
            let status =
                if isRunning then
                    match wipe, DataAccess.Tip.tryGet dataAccess session with
                    | Reset, Some _ ->
                        eventX "Resetting AddressDB"
                        |> Log.info
                        Repository.reset dataAccess session
                    | _, Some _ ->
                        eventX "Syncing AddressDB"
                        |> Log.info
                    | _, None -> 
                        eventX "Creating AddressDB"
                        |> Log.info
                        Repository.init dataAccess session
                        
                    sync dataAccess session client
                else
                    Stop

            Session.commit session

            status

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
            |> Observable.scan (fun status handler ->
                use session = DatabaseContext.createSession databaseContext
                let status = handler dataAccess session status

                Session.commit session

                status
            ) status

        Disposables.fromFunction (fun () ->
            DataAccess.dispose dataAccess
            Disposables.dispose databaseContext), observable
    )
