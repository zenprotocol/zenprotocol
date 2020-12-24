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
open Consensus.Crypto
open Consensus.Types
open Logary.Message
open Api.Helpers
open Consensus.Chain
open FsBech32
open Wallet
open BlockTemplateCache
open Config


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

let publishTransaction
    (config : Config)
    (ex     : TransactionExtended) =
    Blockchain.validateTransaction config.client ex

    match Blockchain.getTransaction config.client ex.txHash with
    | Some (_,0ul) ->
        Hash.toString ex.txHash
        |> JsonValue.String
        |> JsonContent
        |> config.reply StatusCode.OK
    | Some _ ->
        "transaction already exists"
        |> TextContent
        |> config.reply StatusCode.Found
    | _ ->
        match Blockchain.checkTransaction config.client ex with
        | Ok _ ->
            Hash.toString ex.txHash
            |> JsonValue.String
            |> JsonContent
            |> config.reply StatusCode.OK
        | Error error ->
            sprintf "%A" error
            |> config.replyError

let validateBlock
    (config : Config)
    (block  : Types.Block) =
        //TODO: Check correctness
        Blockchain.validateMinedBlock config.client block
        match BlockValidation.validate block (Chain.getChainParameters config.chain) with
        | Ok _ ->
            Block.hash block.header
            |> Hash.toString
            |> JsonValue.String
            |> JsonContent
            |> config.reply StatusCode.OK
        | Error error ->
            error
            |> config.replyError

let parseConfirmations query (config: Config) get =
    match Map.tryFind "confirmations" query with
    | None -> get 0ul
    | Some confirmations ->
        match System.UInt32.TryParse confirmations with
        | true, confirmations -> get confirmations
        | _ -> config.replyError "invalid confirmations"
module Blockchain =

    let headers
        (config: Config)
        (query:Query)
        : unit =
            let tip =
                    Blockchain.getTip config.client
                    |> Option.map snd
                    |> Option.map (fun x -> x.blockNumber)
                    |> Option.defaultValue 0ul
                    |> int
            match parseHeadersRequestJson query tip with
            | Ok (blockNumber, take)->
                tip - blockNumber
                |> Blockchain.getHeaders config.client take
            | Error _ ->
                Blockchain.getAllHeaders config.client
            |> List.map headerEncoder
            |> List.map (fun json -> json.JsonValue)
            |> List.toArray
            |> JsonValue.Array
            |> JsonContent
            |> config.reply StatusCode.OK

    let blocktemplate
        (config: Config)
        (query: Query)
        : unit =
            let pkHash =
                match Map.tryFind "address" query with
                | None -> Wallet.getAddressPKHash config.client
                | Some address -> Address.decodePK config.chain address

            match pkHash with
            | Ok pkHash ->
                match config.templateCache.response(pkHash) with
                | None ->
                    config.reply StatusCode.RequestTimeout NoContent
                | Some template ->
                    config.reply StatusCode.OK template
            | Error error ->
                error
                |> config.replyError
    let block
        (config: Config)
        (query: Query)
        : unit =
            match Map.tryFind "hash" query with
            | None ->
                  match Map.tryFind "blockNumber" query with
                  | Some blockNumber ->
                      match System.UInt32.TryParse blockNumber with
                      | false,_ ->
                          "couldn't decode hash"
                          |> config.replyError
                      | true, blockNumber ->
                          match Blockchain.getBlockByNumber config.client blockNumber with
                          | None ->
                              TextContent (sprintf "block not found")
                              |> config.reply StatusCode.NotFound
                          | Some block ->
                              config.reply StatusCode.OK (JsonContent <| blockEncoder config.chain block)
                  | None ->
                      "hash or blockNumber are missing"
                      |> config.replyError

            | Some h ->
                match Hash.fromString h with
                | None ->
                    "couldn't decode hash"
                    |> config.replyError
                | Some hash ->
                    match Blockchain.getBlock config.client true hash with
                    | None ->
                        TextContent (sprintf "block not found")
                        |> config.reply StatusCode.NotFound
                    | Some block ->
                        config.reply StatusCode.OK (JsonContent <| blockEncoder config.chain block)
    let blocks
        (config: Config)
        (query: Query)
        : unit =
            let blockNumber, take =
                let tip =
                    Blockchain.getTip config.client
                    |> Option.map snd
                    |> Option.map (fun x -> x.blockNumber)
                    |> Option.defaultValue 0ul
                    |> int

                parseHeadersRequestJson query tip
                |> Option.ofResult
                |> Option.map (fun (blockNumber, take) -> (tip - blockNumber, take))
                |> Option.defaultValue (0, tip)

            Blockchain.getBlocks config.client take blockNumber
            |> List.map blockRawEncoder
            |> List.toArray
            |> JsonValue.Array
            |> JsonContent
            |> config.reply StatusCode.OK
    let transaction
        (config: Config)
        (query: Query)
        : unit =
            match Map.tryFind "hash" query with
            | Some hash ->
                let hex = (Map.tryFind "hex" query |> Option.defaultValue "false") = "true"

                Hash.fromString hash
                |> Result.ofOption "could not deserialize Hash"
                |> Result.bind (Blockchain.getTransaction config.client >> ofOption "transaction not found")
                |> Result.map (fun (tx,confirmations) ->
                    let confirmations = "confirmations", (confirmations |> decimal |> JsonValue.Number)

                    if hex then
                        let tx = Transaction.toHex tx

                        [| "tx", JsonValue.String tx; confirmations |]
                        |> JsonValue.Record
                        |> JsonContent
                        |> config.reply StatusCode.OK
                    else
                        let tx = transactionEncoder config.chain tx

                        [| "tx", tx; confirmations |]
                        |> JsonValue.Record
                        |> JsonContent
                        |> config.reply StatusCode.OK)
                |> Result.mapError (TextContent >> config.reply StatusCode.NotFound)
                |> ignore
            | None ->
                config.replyError "transaction not found"

    let publishBlock
        (config: Config)
        (body:string)
        : unit =
            match parsePublishBlockJson body with
            | Error error ->
                config.replyError error
            | Ok block ->
                validateBlock config block

    let publishTransaction
        (config: Config)
        (body:string)
        : unit =
            match Parsing.parseTxHexJson body with
                | Ok ex ->
                    publishTransaction config ex
                | Error error ->
                    config.replyError error

    let submitHeader
        (config: Config)
        (body:string)
        : unit =
            match parseSubmitBlockHeaderJson body with
            | Error error ->
                sprintf "error parsing header, error: %s" error
                |> config.replyError
            | Ok header ->
                match
                    List.tryFind
                        (fun (_, block : Types.Block, _) ->
                            {header with timestamp = 0UL; nonce = (0UL,0UL)} = {block.header with timestamp = 0UL; nonce = (0UL,0UL)})
                        config.templateCache.templates with
                | Some (_, bk, _) ->
                    validateBlock config {bk with header = header}
                | _ -> config.replyError "block not found"
    let contractExecute
        (config: Config)
        (body:string)
        : unit =
            parseContractExecuteFromTransactionJson config.chain body
            >>= (fun (contractId, command, message, tx, sender) ->
                Blockchain.executeContract config.client contractId command sender message tx)
            <@> (Transaction.serialize Full
                >> Base16.encode
                >> TextContent
                >> config.reply StatusCode.OK)
            |> Result.mapError config.replyError
            |> ignore //TODO: CHECK ME

    let info
        (config: Config)
        : unit =
            match Blockchain.tryGetBlockChainInfo config.client with
            | None ->
                eventX "GetBlockChainInfo timedout"
                |> Log.warning
                config.reply StatusCode.RequestTimeout NoContent
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

                config.reply StatusCode.OK (JsonContent json)
    let winner
        (config: Config)
        : unit =
            let interval =
                    Blockchain.getTip config.client
                    |> Option.map snd
                    |> Option.map (fun x -> x.blockNumber)
                    |> Option.defaultValue 0ul
                    |> CGP.getInterval (Chain.getChainParameters config.chain)

            match Blockchain.getWinner config.client with
            | Some ({allocation=Some al ;payout= payout}) ->
                ({allocation= al; payout=payout}:CGP.T)
                |> cgpEncoder config.chain interval
                |> JsonContent
                |> config.reply StatusCode.OK
            | _ ->
                config.replyError "Winner was not found"
    let cgp
        (config: Config)
        : unit =
            match Blockchain.getTip config.client with
            | Some (_,header) ->
                let interval = CGP.getInterval (Chain.getChainParameters config.chain) header.blockNumber
                let cgp = Blockchain.getCgp config.client
                config.reply StatusCode.OK (JsonContent <| (cgpEncoder config.chain interval cgp))
            | None ->
                config.replyError "No tip"

    let cgpHistory
        (config: Config)
        : unit =
            let cgp = Blockchain.getCgpHistory config.client
            config.reply StatusCode.OK (JsonContent <| (cgpHistoryEncoder config.chain cgp))
    let totalZP
        (config: Config)
        : unit =
            Blockchain.getTotalZP config.client
            |> decimal
            |> JsonValue.Number
            |> JsonContent
            |> config.reply StatusCode.OK
    let cgpContract
        (config: Config)
        : unit =
            let cgp = Blockchain.getCgp config.client
            let msgBody =
                Option.map CGP.internalizeRecipient cgp.payout
                |> Option.bind Consensus.CGP.Contract.createPayoutMsgBody
            let raw =
                msgBody
                |> Option.map (dataEncoder config.chain)
                |> Option.defaultValue JsonValue.Null
            let encoded =
                msgBody
                |> Option.map Data.serialize
                |> Option.map Base16.encode
                |> Option.map JsonValue.String
                |> Option.defaultValue JsonValue.Null
            JsonValue.Record
                [|
                    ("raw", raw)
                    ("encoded", encoded)
                    ("cgpContract", (Chain.getChainParameters config.chain).cgpContractId.AsString |> JsonValue.String)
                |]
            |> JsonContent
            |> config.reply StatusCode.OK
    let blockreward
        (config: Config)
        (query:Query)
        : unit =
            match Map.tryFind "blockNumber" query with
            | None -> config.replyError "blockNumber is missing"
            | Some blockNumber ->
                let success,blockNumber = System.UInt32.TryParse blockNumber
                if success then
                    Blockchain.getBlockReward config.client blockNumber
                    |> decimal
                    |> JsonValue.Number
                    |> JsonContent
                    |> config.reply StatusCode.OK
                else
                    config.replyError "invalid blockNumber"
    let candidates
        (config: Config)
        (query:Query)
        : unit =
            query
            |> Map.tryFind "interval"
            |> Option.map uint32
            |> Option.defaultValue (
                    Blockchain.getTip config.client
                    |> Option.map (fun (_,h)-> h.blockNumber)
                    |> Option.defaultValue 0ul
                    |> CGP.getInterval (getChainParameters config.chain)
            )
            |> Blockchain.getCandidates config.client
            |> List.map (payoutEncoder config.chain)
            |> List.toArray
            |> JsonValue.Array
            |> JsonContent
            |> config.reply StatusCode.OK

module AddressDB =
    let balance
        (config: Config)
        (body: string)
        : unit =
            match parseGetBalanceJson config.chain body with
            | Error error -> config.replyError error
            | Ok addresses ->
                match AddressDB.getBalance config.client addresses with
                | Ok balance ->
                    balance
                    |> Map.toSeq
                    |> Seq.map (fun (asset, amount) -> new BalanceResponseJson.Root(Asset.toString asset, int64 amount))
                    |> Seq.map (fun json -> json.JsonValue)
                    |> Seq.toArray
                    |> JsonValue.Array
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error ->
                    config.replyError error
    let transactionCount
        (config: Config)
        (body: string)
        : unit =
            match parseTransactionCountJson config.chain body with
            | Error error -> config.replyError error
            | Ok address ->
                match Blockchain.getTip config.client with
                | Some (_,header) ->
                    match AddressDB.getTransactionCount config.client (address,header.blockNumber) with
                    | Ok count ->
                    count
                    |> decimal
                    |> JsonValue.Number
                    |> JsonContent
                    |> config.reply StatusCode.OK
                    | Error error -> config.replyError error
                | None ->
                config.replyError "No tip"
    let contractMint
        (config: Config)
        (body: string)
        : unit =
            match parseAsset body with
            | Error error ->
                config.replyError error
            | Ok address ->
                match AddressDB.getContractAssets config.client address with
                | Ok (Some (pk,command, messageBody)) ->
                    [|
                        "sender",
                            pk
                            |> Option.map JsonValue.String
                            |> Option.defaultValue JsonValue.Null
                        "command",
                            command
                            |> JsonValue.String
                        "messageBody",
                            messageBody
                            |> Option.map (dataEncoder config.chain)
                            |> Option.defaultValue JsonValue.Null
                        "messageBodyRaw",
                            messageBody
                            |> Option.map Serialization.Data.serialize
                            |> Option.map Base16.encode
                            |> Option.map JsonValue.String
                            |> Option.defaultValue JsonValue.Null
                    |]
                    |> JsonValue.Record
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Ok None ->
                    config.replyError "No data"
                | Error error ->
                    config.replyError error
    let contractHistory
        (config: Config)
        (body: string)
        : unit =
            parseGetContractHistoryJson body
            >>= AddressDB.getContractHistory config.client
            <@> List.map (fun (command, messageBody, txHash, confirmations) ->
                [|
                    "command",
                        command
                        |> JsonValue.String
                    "messageBody",
                        messageBody
                        |> Option.map (dataEncoder config.chain)
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
            <@> config.reply StatusCode.OK
            |> Result.mapError config.replyError
            |> ignore//TODO: check me
    let outputs
        (config: Config)
        (body: string)
        : unit =
            match parseGetOutputsJson config.chain body with
            | Error error -> config.replyError error
            | Ok (addresses, mode) ->
                match AddressDB.getOutputs config.client (addresses, mode) with
                | Ok pointedOutputs ->
                    pointedOutputs
                    |> Seq.map (pointedOutputEncoder config.chain)
                    |> Seq.toArray
                    |> JsonValue.Array
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error ->
                    config.replyError error
    let transactions
        (config: Config)
        (body: string)
        : unit =
            match parseGetHistoryJson config.chain body with
            | Error error -> config.replyError error
            | Ok addresses ->
                match AddressDB.getTransactions config.client addresses with
                | Ok txs ->
                    let json =
                        txs
                        |> List.map (fun (txHash,direction, spend, confirmations, lock) ->
                            let amount = (if direction = TransactionDirection.In then 1L else -1L) * int64 spend.amount

                            transactionHistoryEncoder config.chain txHash spend.asset amount confirmations lock)
                        |> List.toArray
                        |> JsonValue.Array
                    (new TransactionsResponseJson.Root(json)).JsonValue
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error -> config.replyError error
    let resync
        (config: Config)
        : unit =
            AddressDB.resyncAccount config.client
            config.reply StatusCode.OK NoContent

module Wallet =
    let publicKey
        (config: Config)
        (body: string)
        : unit =
        match parseGetPublicKeyJson body with
            | Error error -> config.replyError error
            | Ok (path, password) ->
                match Wallet.getPublicKey config.client path password with
                | Ok key ->
                    PublicKey.toString key
                    |> JsonValue.String
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error ->
                    config.replyError error
    let importWatchOnlyAddress
        (config: Config)
        (body: string)
        : unit =
            match Parsing.parseAddress body with
            | Ok address ->
                match Wallet.importWatchOnlyAddress config.client address with
                | Ok () -> config.reply StatusCode.OK NoContent
                | Error error -> config.replyError error
            | Error error ->
                config.replyError error
    let sign
        (config: Config)
        (body: string)
        : unit =
            match parseSignJson body with
            | Error error -> config.replyError error
            | Ok (message, path, password) ->
                match Wallet.sign config.client message path password with
                | Ok signature ->
                    Signature.toString signature
                    |> JsonValue.String
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error ->
                    config.replyError error
    let balance
        (config: Config)
        : unit =
            match Wallet.getBalance config.client with
            | Ok balances ->
                balances
                |> Map.toSeq
                |> Seq.map (fun (asset, amount) -> new BalanceResponseJson.Root(Asset.toString asset, int64 amount))
                |> Seq.map (fun json -> json.JsonValue)
                |> Seq.toArray
                |> JsonValue.Array
                |> JsonContent
                |> config.reply StatusCode.OK
            | Error error ->
                config.replyError error
    let getNewAddress
        (config: Config)
        : unit =
            match Wallet.getNewAddress config.client with
            | Ok (address,index) ->
                (new ImportAddressResultJson.Root(address,index)).JsonValue
                |> JsonContent
                |> config.reply StatusCode.OK
            | Error error -> config.replyError error
    let restoreNewAddresses
        (config: Config)
        (body: string)
        : unit =
            match parseRestoreNewAddress body with
            | Error error -> config.replyError error
            | Ok max ->
                Wallet.restoreNewAddresses config.client max
                config.reply StatusCode.OK NoContent
    let exist
        (config: Config)
        : unit =
            match Wallet.accountExists config.client with
            | Ok result ->
                result
                |> JsonValue.Boolean
                |> JsonContent
                |> config.reply StatusCode.OK
            | Error error ->
                config.replyError error
    let zenPublicKey
        (config: Config)
        : unit =
            match Wallet.exportZenPublicKey config.client with
            | Ok publicKey ->
                JsonValue.String publicKey
                |> JsonContent
                |> config.reply StatusCode.OK
            | Error error ->
                config.replyError error
    let checkPassword
        (config: Config)
        (body: string)
        : unit =
            match parseCheckPasswordJson body with
            | Ok password ->
                match Wallet.checkPassword config.client password with
                | Ok result ->
                    result
                    |> JsonValue.Boolean
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error ->
                    config.replyError error
            | Error error ->
                config.replyError error
    let changePassword
        (config: Config)
        (body: string)
        : unit =
            match parseChangePassword body with
            | Ok (oldPassword, newPassword) ->
                match Wallet.changeSecure config.client oldPassword newPassword with
                | Ok _ ->
                    "Password changed"
                    |> JsonValue.String
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error ->
                    config.replyError error
            | Error error ->
                config.replyError error

    let import
        (config: Config)
        (body: string)
        : unit =
            match parseImportSeedJson body with
            | Ok (words, key) ->
                match Wallet.importSeed config.client words key with
                | Ok _ -> config.reply StatusCode.OK (TextContent "account imported")
                | Error error -> config.replyError error
            | Error error ->
                config.replyError error
    let mnemonicphrase
        (config: Config)
        (body: string)
        : unit =
            // TODO: should be a get with Authorization header
            match parseCheckPasswordJson body with
            | Ok password ->
                match Wallet.getMnemonicPhrase config.client password with
                | Ok mnemonicPhrase ->
                    mnemonicPhrase
                    |> TextContent
                    |> config.reply StatusCode.OK
                | Error error ->
                    config.replyError error
            | Error error ->
                config.replyError error
    let send
        (config: Config)
        (body: string)
        : unit =
            match parseSendJson config.chain body with
            | Ok (outputs, password) ->
                Wallet.createTransaction config.client true outputs password
                |> handleTxResult config
            | Error error ->
                config.replyError error
    let keys
        (config: Config)
        (body: string)
        : unit =
            match parseCheckPasswordJson body with
            | Ok password ->
                match Wallet.getKeys config.client password with
                | Ok map ->
                    Map.toArray map
                    |> Array.map (fun (publicKey, path) ->
                        new PublicKeyDataJson.Root(PublicKey.toString publicKey, path))
                    |> Array.map (fun json -> json.JsonValue)
                    |> JsonValue.Array
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error ->
                    config.replyError error
            | Error error ->
                config.replyError error
    let createTransaction
        (config: Config)
        (body: string)
        : unit =
            match parseSendJson config.chain body with
            | Ok (outputs, password) ->
                match Wallet.createTransaction config.client false outputs password with
                | Ok tx ->
                    tx
                    |> Transaction.serialize Full
                    |> Base16.encode
                    |> TextContent
                    |> config.reply StatusCode.OK
                | Error error -> config.replyError error
            | Error error ->
                config.replyError error

    let importZenPublicKey
        (config: Config)
        (body: string)
        : unit =
            match parseImportZenPublicKey body with
            | Error error -> config.replyError error
            | Ok publicKey ->
                match Wallet.importZenPublicKey config.client publicKey with
                | Ok _ ->
                    config.reply StatusCode.OK (TextContent "zenKey imported")
                | Error error -> config.replyError error
    let remove
        (config: Config)
        (body: string)
        : unit =
            match parseCheckPasswordJson body with
            | Ok password ->
                match Wallet.removeAccount config.client password with
                | Ok _ -> config.reply StatusCode.OK (TextContent "wallet removed")
                | Error error -> config.replyError error
            | Error error ->
                config.replyError error
    let address
        (config: Config)
        : unit =
            match Wallet.getAddress config.client with
            | Ok address ->
                address
                |> JsonValue.String
                |> JsonContent
                |> config.reply StatusCode.OK
            | Error error ->
                config.replyError error

    let transactionCount
        (config: Config)
        : unit =
            match Wallet.getTransactionCount config.client with
            | Ok count ->
                count
                |> decimal
                |> JsonValue.Number
                |> JsonContent
                |> config.reply StatusCode.OK
            | Error error ->
                config.replyError error
    let utxo
        (config: Config)
        : unit =
            match Wallet.getUtxo config.client with
            | Ok pointedOutputs ->
                pointedOutputs
                |> Seq.map (pointedOutputEncoder config.chain)
                |> Seq.toArray
                |> JsonValue.Array
                |> JsonContent
                |> config.reply StatusCode.OK
            | Error error ->
                config.replyError error
    let transactions
        (config: Config)
        (query:Query)
        : unit =
            match parseTransactionsRequestJson query with
            | Ok (skip, take) ->
                match Wallet.getTransactions config.client skip take with
                | Ok txs ->
                    let json =
                        txs
                        |> List.map (fun (txHash,direction, spend, confirmations, lock) ->
                            let amount = (if direction = TransactionDirection.In then 1L else -1L) * int64 spend.amount

                            transactionHistoryEncoder config.chain txHash spend.asset amount confirmations lock)
                        |> List.toArray
                        |> JsonValue.Array
                    (new TransactionsResponseJson.Root(json)).JsonValue
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error ->
                    config.replyError error
            | Error error ->
                config.replyError error
    let receivedByAddress
        (config: Config)
        (query:Query)
        : unit =
            let get confirmations =
                match Wallet.getReceivedByAddress config.client confirmations with
                | Ok received ->
                    Map.toSeq received
                    |> Seq.map (fun ((address,asset),amount) -> (new ReceivedByAddressJson.Root(address, Asset.toString asset, string amount)).JsonValue)
                    |> Seq.toArray
                    |> JsonValue.Array
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error -> config.replyError error

            parseConfirmations query config get
    let addressOutputs
        (config: Config)
        (query:Query)
        : unit =
            match Map.tryFind "address" query with
            | Some address ->
                match Wallet.getAddressOutputs config.client address with
                | Ok outputs ->
                    outputs
                    |> List.map (fun ((outpoint:Types.Outpoint),(spend:Types.Spend),confirmations,spent) ->
                        new AddressOutputJson.Root(new AddressOutputJson.Outpoint(Hash.toString outpoint.txHash, outpoint.index |> int32),
                            Asset.toString spend.asset, string spend.amount, int confirmations,spent))
                    |> List.map (fun json -> json.JsonValue)
                    |> List.toArray
                    |> JsonValue.Array
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error -> config.replyError error
            | _ -> config.replyError "address is missing"

    let addressBalance
        (config: Config)
        (query:Query)
        : unit =
            match Map.tryFind "address" query with
            | Some address ->
                match Wallet.getAddressOutputs config.client address with
                | Ok outputs ->
                    outputs
                    |> List.map (fun ((outpoint:Types.Outpoint),(spend:Types.Spend),confirmations,spent) ->
                        new AddressOutputJson.Root(new AddressOutputJson.Outpoint(Hash.toString outpoint.txHash, outpoint.index |> int32),
                            Asset.toString spend.asset, string spend.amount, int confirmations,spent))
                    |> List.map (fun json -> json.JsonValue)
                    |> List.toArray
                    |> JsonValue.Array
                    |> JsonContent
                    |> config.reply StatusCode.OK
                | Error error -> config.replyError error
            | None-> config.replyError "address is missing"


    let resync
        (config: Config)
        : unit =
            Wallet.resyncAccount config.client
            config.reply StatusCode.OK NoContent

    module Contract =
        let activate
            (config: Config)
            (body: string)
            =
                match parseContractActivateJson body with
                | Error error -> config.replyError error
                | Ok (code, numberOfBlocks, rlimit, password) ->
                    match Wallet.activateContract config.client true code numberOfBlocks rlimit password with
                    | Ok (tx, contractId) ->
                        let txHash  = Transaction.hash tx
                        let address =
                            Address.Contract contractId
                            |> Address.encode config.chain
                        let json =
                            new ContractActivateOrExtendResponseJson.Root (
                                                                              address,
                                                                              ContractId.toString contractId,
                                                                              txHash.AsString,
                                                                              string numberOfBlocks
                                                                            )
                        config.reply StatusCode.OK (JsonContent json.JsonValue)
                    | Error error ->
                        config.replyError error
        let extend
            (config: Config)
            (body: string)
            =
                match parseContractExtendJson config.chain body with
                | Error error -> config.replyError error
                | Ok (contractId, numberOfBlocks, password) ->
                    match Wallet.extendContract config.client true contractId numberOfBlocks password with
                    | Ok tx ->
                        let txHash  = Transaction.hash tx
                        let address =
                            Address.Contract contractId
                            |> Address.encode config.chain
                        let json =
                            new ContractActivateOrExtendResponseJson.Root (
                                                                              address,
                                                                              ContractId.toString contractId,
                                                                              txHash.AsString,
                                                                              string numberOfBlocks
                                                                            )
                        config.reply StatusCode.OK (JsonContent json.JsonValue)
                    | Error error ->
                        config.replyError error
        let execute
            (config: Config)
            (body: string)
            =
                match parseContractExecuteJson config.chain body with
                | Error error -> config.replyError error
                | Ok (contractId, command, message, returnAddress, sign, spends, password) ->
                    Wallet.executeContract config.client true contractId command message returnAddress sign spends password
                    |> handleTxResult config
        let cgp
            (config: Config)
            (body: string)
            =
                match parseCheckPasswordJson body with
                | Ok (password) ->
                    Wallet.executeCGP config.client true password
                    |> handleTxResult config
                | Error error ->
                    config.replyError error
    module RawTransaction =
        let create
            (config: Config)
            (body: string)
            : unit =
                match parseCreateRawTransactionJson config.chain body with
                | Ok outputs ->
                    Wallet.createRawTransaction config.client outputs
                    |> handleRawTxResult config
                | Error error ->
                    config.replyError error
        let sign
            (config: Config)
            (body: string)
            : unit =
                match parseSignRawTransactionJson body with
                | Ok (tx,password) ->
                    Wallet.signRawTransaction config.client tx password
                    |> handleRawTxResult config
                | Error error ->
                    config.replyError error
        let publish
            (config: Config)
            (body: string)
            : unit =
                match parseRawTransactionJson body with
                | Ok raw ->
                    let tx = Transaction.fromRaw raw

                    if List.length tx.witnesses <> List.length raw.witnesses then
                        config.replyError "raw transaction is not fully signed"
                    else
                        let ex = Transaction.toExtended tx
                        publishTransaction config ex

                | Error error ->
                    config.replyError error
module Contract =

    let contractId
        (config: Config)
        (query:Query)
        : unit =
            match Map.tryFind "address" query with
            | None ->
                  sprintf "address is missing"
                  |> config.replyError
            | Some address ->
                match Address.decodeContract config.chain address with
                | Ok contractId ->
                    TextContent (ContractId.toString contractId)
                    |> config.reply StatusCode.OK
                | Error error ->
                    config.replyError error

    let active
        (config: Config)
        (chain: Chain)
        : unit =
            Blockchain.getActiveContracts config.client
            |> List.map (fun contract ->
                let address = Address.encode chain (Address.Contract contract.contractId)
                new ActiveContractsResponseJson.Root(ContractId.toString contract.contractId, address, contract.expiry |> int,contract.code))
            |> List.map (fun json -> json.JsonValue)
            |> List.toArray
            |> JsonValue.Array
            |> JsonContent
            |> config.reply StatusCode.OK
module Network =
    let connectionCount
        (config: Config)
        : unit =
            Network.getConnectionCount config.client
            |> decimal
            |> JsonValue.Number
            |> JsonContent
            |> config.reply StatusCode.OK

module Address =
        let decode
            (config: Config)
            (query:Query)
            : unit =
                match Map.tryFind "address" query with
                | None -> config.replyError "address is missing"
                | Some address ->
                    match Address.decodeAny config.chain address with
                    | Ok (Address.Contract contractId) ->
                        JsonValue.Record [| "contractId", JsonValue.String <| ContractId.toString contractId|]
                        |> JsonContent
                        |> config.reply StatusCode.OK
                    | Ok (Address.PK pkHash) ->
                        JsonValue.Record [| "pkHash", JsonValue.String <| Hash.toString pkHash|]
                        |> JsonContent
                        |> config.reply StatusCode.OK
                    | Error error ->
                        config.replyError error

let handleRequest (chain:Chain) client (request,reply) (templateCache : BlockTemplateCache) =
    let config = {
        reply         = reply
        replyError    = fun error -> reply StatusCode.BadRequest (TextContent error)
        client        = client
        chain         = chain
        templateCache = templateCache
    }

    match request with
    | Get ("/network/connections/count", _) ->
        Network.connectionCount config
    | Get("/address/decode", query) ->
        Address.decode config query
    | Get("/blockchain/headers", query) ->
        Blockchain.headers config query
    | Get ("/blockchain/info", _) ->
        Blockchain.info config
    | Get ("/blockchain/cgp", _) ->
        Blockchain.cgp config
    | Get ("/blockchain/cgp/history", _) ->
        Blockchain.cgpHistory config
    | Get ("/blockchain/contract/cgp", _) ->
        Blockchain.cgpContract config
    | Post ("/blockchain/publishblock", Some body) ->
       Blockchain.publishBlock config body
    | Post ("/blockchain/submitheader", Some body) ->
        Blockchain.submitHeader config body
    | Get ("/blockchain/blocktemplate", query) ->
        Blockchain.blocktemplate config query
    | Get ("/blockchain/block", query) ->
        Blockchain.block config query
    | Get ("/blockchain/blocks", query) ->
        Blockchain.blocks config query
    | Get ("/blockchain/transaction", query) ->
        Blockchain.transaction config query
    | Post ("/blockchain/publishtransaction", Some body) ->
        Blockchain.publishTransaction config body
    | Post ("/blockchain/contract/execute", Some body) ->
        Blockchain.contractExecute config body
    | Get ("/blockchain/winner", _) ->
        Blockchain.winner config
    | Get("/blockchain/totalzp",_) ->
        Blockchain.totalZP config
    | Get("/blockchain/candidates", query) ->
        Blockchain.candidates config query
    | Get ("/contract/active", _) ->
        Contract.active config chain
    | Get ("/contract/contractId", query) ->
        Contract.contractId config query
    | Post ("/wallet/publickey", Some body) ->
        Wallet.publicKey config body
    | Post ("/wallet/sign", Some body) ->
        Wallet.sign config body
    | Get ("/wallet/balance", _) ->
        Wallet.balance config
    | Get ("/wallet/exists", _) ->
        Wallet.exist config
    | Post ("/wallet/checkpassword", Some body) ->
        Wallet.checkPassword config body
    | Post ("/wallet/changepassword", Some body) ->
        Wallet.changePassword config body
    | Get ("/wallet/address", _) ->
        Wallet.address config
    | Post ("/wallet/import", Some body) ->
        Wallet.import config body
    | Post ("/wallet/mnemonicphrase", Some body) ->
        Wallet.mnemonicphrase config body
    | Post ("/wallet/send", Some body) ->
        Wallet.send config body
    | Post ("/wallet/rawtransaction/create", Some body) ->
        Wallet.RawTransaction.create config body
    | Post ("/wallet/rawtransaction/sign", Some body) ->
        Wallet.RawTransaction.sign config body
    | Post ("/wallet/rawtransaction/publish", Some body) ->
        Wallet.RawTransaction.publish config body
    | Post ("/wallet/keys", Some body) ->
        Wallet.keys config body
    // OBSOLETE: createrawtransaction is obsolete, please use create transaction
    | Post ("/wallet/createrawtransaction", Some body)
    | Post ("/wallet/createtransaction", Some body) ->
        Wallet.createTransaction config body
    | Get ("/wallet/transactioncount", _) ->
        Wallet.transactionCount config
    | Get ("/wallet/transactions", query) ->
        Wallet.transactions config query
    | Post ("/wallet/contract/activate", Some body) ->
        Wallet.Contract.activate config body
    | Post ("/wallet/contract/extend", Some body) ->
        Wallet.Contract.extend config body
    | Post ("/wallet/contract/execute", Some body) ->
        Wallet.Contract.execute config body
    | Post ("/wallet/contract/cgp", Some body) ->
        Wallet.Contract.cgp config body
    | Get  ("/wallet/resync", _)
    | Post ("/wallet/resync", _) ->
        Wallet.resync config
    | Post ("/wallet/importwatchonlyaddress", Some body) ->
        Wallet.importWatchOnlyAddress config body
    | Post ("/wallet/getnewaddress", _) ->
        Wallet.getNewAddress config
    | Get ("/wallet/receivedbyaddress", query) ->
        Wallet.receivedByAddress config query
    | Get ("/wallet/utxo", _) ->
        Wallet.utxo config
    | Get ("/wallet/addressoutputs",query) ->
        Wallet.addressOutputs config query
    | Get("/wallet/addressbalance", query) ->
        Wallet.addressBalance config query
    | Get("/blockchain/blockreward", query) ->
        Blockchain.blockreward config query
    | Post("/wallet/restorenewaddresses", Some body) ->
        Wallet.restoreNewAddresses config body
    | Get("/wallet/zenpublickey", _) ->
        Wallet.zenPublicKey config
    | Post("/wallet/importzenpublickey", Some body) ->
       Wallet.importZenPublicKey config body
    | Post("/wallet/remove", Some body) ->
        Wallet.remove config body
    | Get  ("/addressdb/resync", _)
    | Post ("/addressdb/resync", _) ->
        AddressDB.resync config
    | Post("/addressdb/balance", Some body) ->
        AddressDB.balance config body
    | Post("/addressdb/outputs", Some body) ->
        AddressDB.outputs config body
    | Post("/addressdb/transactions", Some body) ->
       AddressDB.transactions config body
    | Post("/addressdb/transactioncount", Some body) ->
        AddressDB.transactionCount config body
    | Post("/addressdb/contract/mint", Some body) ->
        AddressDB.contractMint config body
    | Post("/addressdb/contract/history", Some body) ->
        AddressDB.contractHistory config body
    | _ ->
        config.replyError "unmatched request"

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
