module Api.Server

open FSharp.Data
open Consensus
open Serialization
open Infrastructure
open ServiceBus
open Http
open Api.Types
open Parsing
open Messaging.Services
open Messaging.Services.Wallet
open Infrastructure.Result
open Zen.Crypto
open Consensus.Crypto
open Logary.Message
open Api.Helpers
open FSharp.Data
open FsBech32
open Hash
open Wallet

[<Literal>]
let BlockTemplateCacheSize = 500

type BlockTemplateCache(client) =
    let mutable templateList : (Hash * Types.Block * Content) list = []
    let mutable newBlockSeen = true
    let getTemplate hash =
        match Blockchain.tryGetBlockTemplate client hash with
        | None ->
            eventX "GetBlockTemplate timedout"
            |> Log.warning
            None
        | Some block ->
            let bytes = Block.serialize block

            // 100 bytes are the header
            let header,body = Array.splitAt Block.HeaderSize bytes

            let parent = Hash.toString block.header.parent
            let target = Difficulty.uncompress block.header.difficulty |> Hash.toString
            let header = FsBech32.Base16.encode header
            let body = FsBech32.Base16.encode body

            let newResponse =
                BlockTemplateJson.Root(header, body, target, parent, block.header.blockNumber |> int)
                |> fun x -> x.JsonValue
                |> JsonContent

            Some (block, newResponse)
    member this.templates
        with get() = templateList
    static member val timestamp : Timestamp.Timestamp = Timestamp.now()
        with get, set
    member this.stale
        with get() =
            newBlockSeen || (Timestamp.now() - BlockTemplateCache.timestamp > 10_000UL)
    member this.response(hash:Hash) =
        if this.stale
        then
            match getTemplate hash with
            | Some (block, response) ->
                templateList <- [(hash, block, response)]
                newBlockSeen <- false
                BlockTemplateCache.timestamp <- Timestamp.now()
                Some response
            | None ->
                templateList <- []
                None
        else
            match List.tryFind (fun (h,_,_) -> h = hash) templateList with
            | None ->
                match getTemplate hash with
                | Some (block, response) ->
                    templateList <- List.truncate BlockTemplateCacheSize <| (hash, block, response) :: templateList
                    Some response
                | None -> None
            | Some (_, _, response ) -> Some response
    member this.newTip() =
        newBlockSeen <- true

type T =
    {
        client: Client.T
        agent: Server.T
        observable: System.IObservable<T->T>
        templateCache: BlockTemplateCache
    }
    interface System.IDisposable with
        member x.Dispose() =
            (x.client :> System.IDisposable).Dispose()
            (x.agent :> System.IDisposable).Dispose()

let parseConfirmations query reply get =
    match Map.tryFind "confirmations" query with
    | None -> get 0ul
    | Some confirmations ->
        match System.UInt32.TryParse confirmations with
        | true, confirmations -> get confirmations
        | _ -> reply StatusCode.BadRequest (TextContent "invalid confirmations")

let publishTransaction client reply ex =
    Blockchain.validateTransaction client ex

    match Blockchain.getTransaction client ex.txHash with
    | Some (_,0ul) ->
        Hash.toString ex.txHash
        |> JsonValue.String
        |> JsonContent
        |> reply StatusCode.OK
    | Some _ ->
        "transaction already exists"
        |> TextContent
        |> reply StatusCode.Found
    | _ ->
        match Blockchain.checkTransaction client ex with
        | Ok _ ->
            Hash.toString ex.txHash
            |> JsonValue.String
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            sprintf "%A" error
            |> TextContent
            |> reply StatusCode.BadRequest

let handleRequest chain client (request,reply) (templateCache : BlockTemplateCache) =
    let replyError error =
        reply StatusCode.BadRequest (TextContent error)

    let handleTxResult result =
        match result with
        | Error error ->
            replyError error
        | Ok tx ->
            Transaction.hash tx
            |> Hash.toString
            |> JsonValue.String
            |> JsonContent
            |> reply StatusCode.OK

    let handleRawTxResult result =
        match result with
        | Error error ->
            replyError error
        | Ok raw ->
            let txHash = Transaction.fromRaw raw |> Transaction.hash |> Hash.toString
            let hex = Serialization.RawTransaction.toHex raw

            (new RawTransactionResultJson.Root(txHash, hex)).JsonValue
            |> JsonContent
            |> reply StatusCode.OK

    let validateBlock block =
        Blockchain.validateMinedBlock client block
        match Block.validate (Chain.getChainParameters chain) block with
        | Ok _ ->
            Block.hash block.header
            |> Hash.toString
            |> JsonValue.String
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            error
            |> TextContent
            |> reply StatusCode.BadRequest

    match request with
    | Get ("/network/connections/count", _) ->
        let count = Network.getConnectionCount client

        reply StatusCode.OK (JsonContent (JsonValue.Number (count |> decimal)))

    | Get ("/blockchain/headers", _) ->
        let headers = Blockchain.getHeaders client

        let json =
            headers
            |> List.map (fun header ->
                let hash = Block.hash header
                new HeadersResponseJson.Root(
                                                Hash.toString hash, header.timestamp |> int64,
                                                Timestamp.toString header.timestamp,
                                                header.blockNumber |> int,
                                                "0x" + header.difficulty.ToString("x"),
                                                Difficulty.uncompress header.difficulty |> Hash.toString
                                                ))
            |> List.map (fun json -> json.JsonValue)
            |> List.toArray
            |> JsonValue.Array

        reply StatusCode.OK (JsonContent json)

    | Get ("/blockchain/info", _) ->
        match Blockchain.tryGetBlockChainInfo client with
        | None ->
            eventX "GetBlockChainInfo timedout"
            |> Log.warning

            reply StatusCode.RequestTimeout NoContent
        | Some info ->
            let json = (
                new BlockChainInfoJson.Root(
                    info.chain,
                    info.blocks|> int,
                    info.headers |> int,
                    info.difficulty |> decimal,
                    info.medianTime |> int64,
                    info.initialBlockDownload,
                    info.tipBlockHash |> Hash.toString)).JsonValue

            reply StatusCode.OK (JsonContent json)

    | Get ("/contract/active", _) ->
        let activeContracts = Blockchain.getActiveContracts client
        let json =
            activeContracts
            |> List.map (fun contract ->
                let address = Address.encode chain (Address.Contract contract.contractId)
                new ActiveContractsResponseJson.Root(ContractId.toString contract.contractId, address, contract.expiry |> int,contract.code))
            |> List.map (fun json -> json.JsonValue)
            |> List.toArray
            |> JsonValue.Array

        reply StatusCode.OK (JsonContent json)
    | Get ("/contract/contractId", query) ->
        match Map.tryFind "address" query with
        | None ->
              TextContent (sprintf "address is missing")
              |> reply StatusCode.BadRequest
        | Some address ->
            match Address.decodeContract chain address with
            | Ok contractId ->
                TextContent (ContractId.toString contractId)
                |> reply StatusCode.OK
            | Error _ ->
                TextContent (sprintf "invalid address %A" query)
                |> reply StatusCode.BadRequest
    | Post ("/wallet/publickey", Some body) ->
        match parseGetPublicKeyJson body with
        | Error error -> replyError error
        | Ok (path, password) ->
            match Wallet.getPublicKey client path password with
            | Ok key ->
                PublicKey.toString key
                |> JsonValue.String
                |> JsonContent
                |> reply StatusCode.OK
            | Error error ->
                TextContent error
                |> reply StatusCode.BadRequest
    | Post ("/wallet/sign", Some body) ->
        match parseSignJson body with
        | Error error -> replyError error
        | Ok (message, path, password) ->
            match Wallet.sign client message path password with
            | Ok signature ->
                Signature.toString signature
                |> JsonValue.String
                |> JsonContent
                |> reply StatusCode.OK
            | Error error ->
                TextContent error
                |> reply StatusCode.BadRequest

    | Get ("/wallet/balance", _) ->
        match Wallet.getBalance client with
        | Ok balances ->
            balances
            |> Map.toSeq
            |> Seq.map (fun (asset, amount) -> new BalanceResponseJson.Root(Asset.toString asset, int64 amount))
            |> Seq.map (fun json -> json.JsonValue)
            |> Seq.toArray
            |> JsonValue.Array
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            replyError error
    | Get ("/wallet/exists", _) ->
        match Wallet.accountExists client with
        | Ok result ->
            result
            |> JsonValue.Boolean
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            replyError error
    | Post ("/wallet/checkpassword", Some body) ->
        match parseCheckPasswordJson body with
        | Ok password ->
            match Wallet.checkPassword client password with
            | Ok result ->
                result
                |> JsonValue.Boolean
                |> JsonContent
                |> reply StatusCode.OK
            | Error error ->
                replyError error
        | Error error ->
            replyError error
    | Get ("/wallet/address", _) ->
        match Wallet.getAddress client with
        | Ok address ->
            address
            |> JsonValue.String
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            replyError error
    | Post ("/wallet/import", Some body) ->
        match parseImportSeedJson body with
        | Ok (words, key) ->
            match Wallet.importSeed client words key with
            | Ok _ -> reply StatusCode.OK NoContent
            | Error error -> replyError error
        | Error error ->
            replyError error
    | Post ("/wallet/mnemonicphrase", Some body) ->
        // TODO: should be a get with Authorization header
        match parseCheckPasswordJson body with
        | Ok password ->
            match Wallet.getMnemonicPhrase client password with
            | Ok mnemonicPhrase ->
                mnemonicPhrase
                |> JsonValue.String
                |> JsonContent
                |> reply StatusCode.OK
            | Error error ->
                replyError error
        | Error error ->
            replyError error
    | Post ("/wallet/send", Some body) ->
        match parseSendJson chain body with
        | Ok (outputs, password) ->
            Wallet.createTransaction client true outputs password
            |> handleTxResult
        | Error error ->
            replyError error
    | Post ("/wallet/vote/allocation", Some body) ->
        match parseVoteAllocationJson body with
        | Ok (allocation, password) ->
            Wallet.createVoteTransaction client true allocation password
            |> handleTxResult
        | Error error ->
            replyError error
    | Post ("/wallet/vote/payout", Some body) ->
        match parseVotePayoutJson chain body with
        | Ok (payout, password) ->
            Wallet.createVoteTransaction client true payout password
            |> handleTxResult
        | Error error ->
            replyError error
    | Post ("/wallet/rawtransaction/create", Some body) ->
        match parseCreateRawTransactionJson chain body with
        | Ok outputs ->
            Wallet.createRawTransaction client outputs
            |> handleRawTxResult
        | Error error ->
            replyError error
    | Post ("/wallet/rawtransaction/sign", Some body) ->
        match parseSignRawTransactionJson body with
        | Ok (tx,password) ->
            Wallet.signRawTransaction client tx password
            |> handleRawTxResult
        | Error error ->
            replyError error
    | Post ("/wallet/rawtransaction/publish", Some body) ->
        match parseRawTransactionJson body with
        | Ok raw ->
            let tx = Transaction.fromRaw raw

            if List.length tx.witnesses <> List.length raw.witnesses then
                replyError "raw transaction is not fully signed"
            else
                let ex = Transaction.toExtended tx
                publishTransaction client reply ex

        | Error error ->
            replyError error
    | Post ("/wallet/keys", Some body) ->
        match parseCheckPasswordJson body with
        | Ok password ->
            match Wallet.getKeys client password with
            | Ok map ->
                Map.toArray map
                |> Array.map (fun (publicKey, path) ->
                    new PublicKeyDataJson.Root(PublicKey.toString publicKey, path))
                |> Array.map (fun json -> json.JsonValue)
                |> JsonValue.Array
                |> JsonContent
                |> reply StatusCode.OK
            | Error error ->
                replyError error
        | Error error ->
            replyError error
    // OBSOLETE: createrawtransaction is obsolete, please use create transaction
    | Post ("/wallet/createrawtransaction", Some body)
    | Post ("/wallet/createtransaction", Some body) ->
        match parseSendJson chain body with
        | Ok (outputs, password) ->
            match Wallet.createTransaction client false outputs password with
            | Ok tx ->
                tx
                |> Transaction.serialize Full
                |> Base16.encode
                |> TextContent
                |> reply StatusCode.OK
            | Error error -> replyError error
        | Error error ->
            replyError error
    | Get ("/wallet/transactioncount", _) ->
        match Wallet.getTransactionCount client with
        | Ok count ->
                    count
                    |> decimal
                    |> JsonValue.Number
                    |> JsonContent
                    |> reply StatusCode.OK
        | Error error ->
                    replyError error
    | Get ("/wallet/transactions", query) ->
        match parseTransactionsRequestJson query with
        | Ok (skip, take) ->
            match Wallet.getTransactions client skip take with
            | Ok txs ->
                let json =
                    txs
                    |> List.map (fun (txHash,direction, spend, confirmations, lock) ->
                        let amount = (if direction = TransactionDirection.In then 1L else -1L) * int64 spend.amount

                        transactionHistoryEncoder chain txHash spend.asset amount confirmations lock)
                    |> List.toArray
                    |> JsonValue.Array
                (new TransactionsResponseJson.Root(json)).JsonValue
                |> JsonContent
                |> reply StatusCode.OK
            | Error error ->
                replyError error
        | Error error ->
            replyError error
    | Post ("/wallet/contract/activate", Some body) ->
        match parseContractActivateJson body with
        | Error error -> replyError error
        | Ok (code, numberOfBlocks, password) ->
            match Wallet.activateContract client true code numberOfBlocks password with
            | Ok (tx, contractId) ->
                let address =
                    Address.Contract contractId
                    |> Address.encode chain
                let json = new ContractActivateResponseJson.Root (address, ContractId.toString contractId)
                reply StatusCode.OK (JsonContent json.JsonValue)
            | Error error ->
                replyError error
    | Post ("/wallet/contract/extend", Some body) ->
        match parseContractExtendJson chain body with
        | Error error -> replyError error
        | Ok (contractId, numberOfBlocks, password) ->
            match Wallet.extendContract client true contractId numberOfBlocks password with
            | Ok tx ->
                reply StatusCode.OK NoContent
            | Error error ->
                replyError error
    | Post ("/wallet/contract/execute", Some body) ->
        match parseContractExecuteJson chain body with
        | Error error -> replyError error
        | Ok (contractId, command, message, returnAddress, sign, spends, password) ->
            Wallet.executeContract client true  contractId command message returnAddress sign spends password
            |> handleTxResult
    | Get ("/wallet/resync", _)
    | Post ("/wallet/resync", _) ->
        Wallet.resyncAccount client
        reply StatusCode.OK NoContent
    | Post ("/blockchain/publishblock", Some body) ->
        match parsePublishBlockJson body with
        | Error error ->
            printfn "error deserializing block"
            replyError error
        | Ok block ->
            validateBlock block
    | Post ("/blockchain/submitheader", Some body) ->
        match parseSubmitBlockHeaderJson body with
        | Error error ->
            printfn "error parsing header"
            replyError error
        | Ok header ->
            match
                List.tryFind
                    (fun (_, block : Types.Block, _) ->
                        {header with timestamp = 0UL; nonce = (0UL,0UL)} = {block.header with timestamp = 0UL; nonce = (0UL,0UL)})
                    templateCache.templates with
            | Some (_, bk, _) ->
                validateBlock {bk with header = header}
            | _ -> TextContent (sprintf "block not found") |> reply StatusCode.BadRequest
    | Get ("/blockchain/blocktemplate", query) ->
        let pkHash =
            match Map.tryFind "address" query with
            | None -> Wallet.getAddressPKHash client
            | Some address -> Address.decodePK chain address

        match pkHash with
        | Ok pkHash ->
            match templateCache.response(pkHash) with
            | None ->
                reply StatusCode.RequestTimeout NoContent
            | Some template ->
                reply StatusCode.OK template
        | Error _ ->
            TextContent (sprintf "invalid address %A" query)
            |> reply StatusCode.BadRequest
    | Get ("/blockchain/block", query) ->
        match Map.tryFind "hash" query with
        | None ->
              match Map.tryFind "blockNumber" query with
              | Some blockNumber ->
                  match System.UInt32.TryParse blockNumber with
                  | false,_ ->
                      TextContent (sprintf "couldn't decode hash")
                      |> reply StatusCode.BadRequest
                  | true, blockNumber ->
                      match Blockchain.getBlockByNumber client blockNumber with
                      | None ->
                          TextContent (sprintf "block not found")
                          |> reply StatusCode.NotFound
                      | Some block ->
                          reply StatusCode.OK (JsonContent <| blockEncoder chain (Block.hash block.header) block)
              | None ->
                  TextContent (sprintf "hash or blockNumber are missing")
                  |> reply StatusCode.BadRequest

        | Some h ->
            match Hash.fromString h with
            | Error _ ->
                TextContent (sprintf "couldn't decode hash")
                |> reply StatusCode.BadRequest
            | Ok hash ->
                match Blockchain.getBlock client true hash with
                | None ->
                    TextContent (sprintf "block not found")
                    |> reply StatusCode.NotFound
                | Some block ->
                    reply StatusCode.OK (JsonContent <| blockEncoder chain hash block)

    | Get ("/blockchain/transaction", query) ->
        match Map.tryFind "hash" query with
        | Some hash ->
            let hex = (Map.tryFind "hex" query |> Option.defaultValue "false") = "true"

            Hash.fromString hash
            |> Result.bind (Blockchain.getTransaction client >> ofOption "transaction not found")
            |> Result.map (fun (tx,confirmations) ->
                let confirmations = "confirmations", (confirmations |> decimal |> JsonValue.Number)

                if hex then
                    let tx = Transaction.toHex tx

                    [| "tx", JsonValue.String tx; confirmations |]
                    |> JsonValue.Record
                    |> JsonContent
                    |> reply StatusCode.OK
                else
                    let tx = transactionEncoder chain tx

                    [| "tx", tx; confirmations |]
                    |> JsonValue.Record
                    |> JsonContent
                    |> reply StatusCode.OK)

            |> Result.mapError (TextContent >> reply StatusCode.NotFound)
            |> ignore
        | None ->
            reply StatusCode.BadRequest NoContent
    | Post ("/blockchain/publishtransaction", Some tx) ->
        match Parsing.parseTxHexJson tx with
        | Ok ex ->
            publishTransaction client reply ex

        | Error error ->
            reply StatusCode.BadRequest <| TextContent error
    | Post ("/blockchain/contract/execute", Some body) ->
        parseContractExecuteFromTransactionJson chain body
        >>= (fun (contractId, command, message, tx, sender) ->
            Blockchain.executeContract client contractId command sender message tx)
        <@> (Transaction.serialize Full
            >> Base16.encode
            >> TextContent
            >> reply StatusCode.OK)
        |> Result.mapError replyError
        |> ignore
    | Post ("/wallet/importwatchonlyaddress", Some json) ->
        match Parsing.parseAddress json with
        | Ok address ->
            match Wallet.importWatchOnlyAddress client address with
            | Ok () -> reply StatusCode.OK NoContent
            | Error error -> reply StatusCode.BadRequest (TextContent error)
        | Error error ->
            reply StatusCode.BadRequest <| TextContent error
    | Post ("/wallet/getnewaddress", _) ->
        match Wallet.getNewAddress client with
        | Ok (address,index) ->
            (new ImportAddressResultJson.Root(address,index)).JsonValue
            |> JsonContent
            |> reply StatusCode.OK
        | Error error -> reply StatusCode.BadRequest (TextContent error)
    | Get ("/wallet/receivedbyaddress", query) ->
        let get confirmations =
            match Wallet.getReceivedByAddress client confirmations with
            | Ok received ->
                Map.toSeq received
                |> Seq.map (fun ((address,asset),amount) -> (new ReceivedByAddressJson.Root(address, Asset.toString asset, int64 amount)).JsonValue)
                |> Seq.toArray
                |> JsonValue.Array
                |> JsonContent
                |> reply StatusCode.OK
            | Error error -> reply StatusCode.BadRequest (TextContent error)

        parseConfirmations query reply get

    | Get ("/wallet/addressoutputs",query) ->
        match Map.tryFind "address" query with
        | Some address ->
            match Wallet.getAddressOutputs client address with
            | Ok outputs ->
                outputs
                |> List.map (fun ((outpoint:Types.Outpoint),(spend:Types.Spend),confirmations,spent) ->
                    new AddressOutputJson.Root(new AddressOutputJson.Outpoint(Hash.toString outpoint.txHash, outpoint.index |> int32),
                        Asset.toString spend.asset, spend.amount |> int64, int confirmations,spent))
                |> List.map (fun json -> json.JsonValue)
                |> List.toArray
                |> JsonValue.Array
                |> JsonContent
                |> reply StatusCode.OK
            | Error error -> reply StatusCode.BadRequest (TextContent error)
        | _ -> reply StatusCode.BadRequest (TextContent "address is missing")
    | Get("/address/decode", query) ->
        match Map.tryFind "address" query with
        | None -> reply StatusCode.BadRequest (TextContent "address is missing")
        | Some address ->
            match Address.decodeAny chain address with
            | Ok (Address.Contract contractId) ->
                JsonValue.Record [| "contractId", JsonValue.String <| ContractId.toString contractId|]
                |> JsonContent
                |> reply StatusCode.OK
            | Ok (Address.PK pkHash) ->
                JsonValue.Record [| "pkHash", JsonValue.String <| Hash.toString pkHash|]
                |> JsonContent
                |> reply StatusCode.OK
            | Error error ->
                reply StatusCode.BadRequest (TextContent error)
    | Get("/wallet/addressbalance", query) ->
        let get confirmations =
            match Map.tryFind "address" query with
            | Some address ->
                match Wallet.getAddressBalance client address confirmations with
                | Ok balances ->
                    balances
                    |> Map.toSeq
                    |> Seq.map (fun (asset,amount) -> new SpendJson.Root(Asset.toString asset, int64 amount))
                    |> Seq.map (fun json -> json.JsonValue)
                    |> Seq.toArray
                    |> JsonValue.Array
                    |> JsonContent
                    |> reply StatusCode.OK
                | Error error -> reply StatusCode.BadRequest (TextContent error)
            | _ -> reply StatusCode.BadRequest (TextContent "address is missing")
        parseConfirmations query reply get
    | Get("/blockchain/blockreward", query) ->
        match Map.tryFind "blockNumber" query with
        | None -> reply StatusCode.BadRequest (TextContent "blockNumber is missing")
        | Some blockNumber ->
            let success,blockNumber = System.UInt32.TryParse blockNumber
            if success then
                Blockchain.getBlockReward client blockNumber
                |> decimal
                |> JsonValue.Number
                |> JsonContent
                |> reply StatusCode.OK
            else
                reply StatusCode.BadRequest (TextContent "invalid blockNumber")
    | Post("/wallet/restorenewaddresses", Some json) ->
        match parseRestoreNewAddress json with
        | Error error -> reply StatusCode.BadRequest (TextContent error)
        | Ok max ->
            Wallet.restoreNewAddresses client max
            reply StatusCode.OK NoContent
    | Get("/wallet/zenpublickey", _) ->
        match Wallet.exportZenPublicKey client with
        | Ok publicKey ->
            JsonValue.String publicKey
            |> JsonContent
            |> reply StatusCode.OK
        | Error error ->
            TextContent error
            |> reply StatusCode.BadRequest
    | Post("/wallet/importzenpublickey", Some json) ->
        match parseImportZenPublicKey json with
        | Error error -> reply StatusCode.BadRequest (TextContent error)
        | Ok publicKey ->
            match Wallet.importZenPublicKey client publicKey with
            | Ok _ ->
                reply StatusCode.OK NoContent
            | Error error -> reply StatusCode.BadRequest (TextContent error)
    | Post("/wallet/remove", Some json) ->
        match parseCheckPasswordJson json with
        | Ok password ->
            match Wallet.removeAccount client password with
            | Ok _ -> reply StatusCode.OK NoContent
            | Error error -> replyError error
        | Error error ->
            replyError error
    | Post("/addressdb/balance", Some json) ->
        match parseGetBalanceJson chain json with
        | Error error -> reply StatusCode.BadRequest (TextContent error)
        | Ok addresses ->
            match AddressDB.getBalance client addresses with
            | Ok balance ->
                balance
                |> Map.toSeq
                |> Seq.map (fun (asset, amount) -> new BalanceResponseJson.Root(Asset.toString asset, int64 amount))
                |> Seq.map (fun json -> json.JsonValue)
                |> Seq.toArray
                |> JsonValue.Array
                |> JsonContent
                |> reply StatusCode.OK
            | Error error -> reply StatusCode.BadRequest (TextContent error)
    | Post("/addressdb/outputs", Some json) ->
        match parseGetOutputsJson chain json with
        | Error error -> reply StatusCode.BadRequest (TextContent error)
        | Ok (addresses, mode) ->
            match AddressDB.getOutputs client (addresses, mode) with
            | Ok pointedOutputs ->
                pointedOutputs
                |> Seq.map (pointedOutputEncoder chain)
                |> Seq.toArray
                |> JsonValue.Array
                |> JsonContent
                |> reply StatusCode.OK
            | Error error -> reply StatusCode.BadRequest (TextContent error)
    | Post("/addressdb/transactions", Some json) ->
        match parseGetHistoryJson chain json with
        | Error error -> reply StatusCode.BadRequest (TextContent error)
        | Ok addresses ->
            match AddressDB.getTransactions client addresses with
            | Ok txs ->
                let json =
                    txs
                    |> List.map (fun (txHash,direction, spend, confirmations, lock) ->
                        let amount = (if direction = TransactionDirection.In then 1L else -1L) * int64 spend.amount

                        transactionHistoryEncoder chain txHash spend.asset amount confirmations lock)
                    |> List.toArray
                    |> JsonValue.Array
                (new TransactionsResponseJson.Root(json)).JsonValue
                |> JsonContent
                |> reply StatusCode.OK
            | Error error -> reply StatusCode.BadRequest (TextContent error)
    | Get("/blockchain/totalzp",_) ->
        Blockchain.getTotalZP client
        |> decimal
        |> JsonValue.Number
        |> JsonContent
        |> reply StatusCode.OK
    | Post("/addressdb/contract/history", Some json) ->
        parseGetContractHistoryJson json
        >>= AddressDB.getContractHistory client
        <@> List.map (fun (command, messageBody, txHash, confirmations) ->
            [|
                "command",
                    command
                    |> JsonValue.String
                "messageBody",
                    messageBody
                    |> Option.map (dataEncoder chain)
                    |> Option.defaultValue JsonValue.Null
                "txHash",
                    txHash
                    |> Hash.toString
                    |> JsonValue.String
                "confirmations",
                    confirmations
                    |> decimal
                    |> JsonValue.Number
            |]
            |> JsonValue.Record
        )
        <@> List.toArray
        <@> JsonValue.Array
        <@> JsonContent
        <@> reply StatusCode.OK
        |> Result.mapError replyError
        |> ignore
    | _ ->
        reply StatusCode.BadRequest (TextContent "unmatched request")

let create chain poller busName bind =
    let httpAgent = Http.Server.create poller bind

    eventX "Api running on {bind}"
    >> setField "bind" (sprintf "http://%s" bind)
    |> Log.info

    let client = Client.create busName
    let blockTemplateCache = BlockTemplateCache client

    let observable =
        Http.Server.observable httpAgent
        |> Observable.map (fun request ->
            fun (server:T) ->
                handleRequest chain client request blockTemplateCache
                server
        )

    {agent = httpAgent; observable = observable; client = client; templateCache = blockTemplateCache}

let observable server = server.observable