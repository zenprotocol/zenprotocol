module Api.Parsing

open Api
open FSharp.Data
open Api.Types
open Consensus
open Serialization
open Serialization.Serialization
open Crypto
open Types
open FsBech32
open Newtonsoft.Json
open Wallet
open Infrastructure
open Infrastructure.Result
open Infrastructure.Http
open Config

let private getSpend asset amount =
    match Asset.fromString asset with
    | Some asset -> Ok { asset = asset; amount = uint64 amount }
    | None -> Error "invalid asset"

let parseSendJson chain json =
    try
        let json = SendRequestJson.Parse json
        let outputs, errors =
            json.Outputs
            |> Array.fold (fun (outs, errs) output ->
                match Address.decodeAny chain output.Address with
                | Error err ->
                    (outs, ("Address is invalid: " + err) :: errs)
                | Ok (Address.PK pkHash) ->
                    match getSpend output.Asset output.Amount with
                    | Ok spend -> ((PK pkHash, spend) :: outs, errs)
                    | Error err -> (outs, err :: errs)
                | Ok (Address.Contract cId) ->
                    match getSpend output.Asset output.Amount with
                    | Ok spend -> ((Contract cId, spend) :: outs, errs)
                    | Error err -> (outs, err :: errs)) (List.empty, List.empty)
        
        let errors = 
            if String.length json.Password = 0 then
                "Password is empty" :: errors
            else if json.Outputs.Length = 0 then
                "Outputs is empty" :: errors
            else errors
        
        match errors with
        | [] -> Ok (List.rev outputs, json.Password)
        | _  -> Error (String.concat " " errors)
    with ex ->
        Error ("Json invalid: " + ex.Message)


let parseCreateRawTransactionJson chain json =
    try
        let json = CreateRawTransactionJson.Parse json
        let outputs, errors =
            json.Outputs
            |> Array.fold (fun (outs, errs) output ->
                match Address.decodePK chain output.Address with
                | Error err ->
                    (outs, ("Address is invalid: " + err) :: errs)
                | Ok pkHash ->
                    match getSpend output.Asset output.Amount with
                    | Ok spend ->
                        ((pkHash, spend) :: outs, errs)
                    | Error err ->
                        (outs, err :: errs)) (List.empty, List.empty)
        
        match errors with
        | [] when json.Outputs.Length <> 0 -> Ok (List.rev outputs)
        | _ -> 
            let errors = 
                if json.Outputs.Length = 0 then
                    "Outputs is empty" :: errors
                else errors
            Error (String.concat " " errors)
    with ex ->
        Error ("Json invalid: " + ex.Message)


let parseSignRawTransactionJson json =
    try
        let json = SignRawTransactionJson.Parse json

        if String.length json.Password = 0 then
            Error "Password is empty"
        else

        match Serialization.RawTransaction.fromHex json.Tx with
        | Some tx ->
            Ok (tx, json.Password)
        | None ->
            Error "Invalid tx: decoding"
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)

let parseRawTransactionJson json =
    try
        let json = TxHexJson.Parse json

        match Serialization.RawTransaction.fromHex json.Tx with
            | Some tx ->
                Ok tx
            | None ->
                Error "Invalid tx: decoding"
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)

let parseContractExecuteJson chain json =
    try
        let json = ContractExecuteRequestJson.Parse json

        if String.length json.Password = 0 then
            Error "Password is empty"
        else
            match Address.decodeContract chain json.Address with
            | Error err -> Error ("Address is invalid: " + err)
            | Ok contractId ->
                let spends, errors =
                    json.Spends
                    |> Array.fold (fun (spends, errs) item ->
                        match getSpend item.Asset item.Amount with
                        | Ok spend -> (Map.add spend.asset spend.amount spends, errs)
                        | Error err -> (spends, err :: errs)) (Map.empty, List.empty)

                if List.isEmpty errors then
                    let messageBody =
                        if System.String.IsNullOrEmpty json.MessageBody then None
                        else
                            match Base16.decode json.MessageBody with
                            | Some data ->
                                Serialization.Data.deserialize data
                            | None -> failwith "Invalid Data"
                    
                    let sign =
                        if System.String.IsNullOrEmpty json.Options.Sign then
                            None
                        else
                            Some json.Options.Sign

                    Ok (contractId, json.Command, messageBody, json.Options.ReturnAddress, sign, spends, json.Password)
                else
                    errors
                    |> String.concat " "
                    |> Error
    with ex ->
        Error ("Json is invalid: " + ex.Message)

let parseContractExecuteFromTransactionJson chain json =
    Exception.resultWrap<ContractExecuteFromTransactionJson.Root> (fun _ -> ContractExecuteFromTransactionJson.Parse json) "Invalid JSON"
    >>= (fun json ->
        Address.decodeContract chain json.Address
        >>= fun address -> Ok (json, address))
    >>= (fun (json, contractId) ->
        json.Tx
        |> Base16.decode
        |> Result.ofOption "Invalid tx: decoding"
        >>= (TxSkeleton.deserialize
            >> Result.ofOption "Invalid tx: deserializing")
        >>= fun tx -> Ok (json, contractId, tx))
    >>= (fun (json, contractId, tx) ->
        (if System.String.IsNullOrEmpty json.MessageBody then
            Ok None
        else
            json.MessageBody
            |> Base16.decode
            |> Result.ofOption "Invalid data: decoding"
            >>= (Data.deserialize
                >> Result.ofOption "Invalid data: deserializing")
                <@> Some)
        <@> (fun messageBody -> json, contractId, tx, messageBody))
    >>= (fun (json, contractId, tx, messageBody) ->
        (if System.String.IsNullOrEmpty json.Options.Sender then
            Ok None
        else
            json.Options.Sender
            |> Base16.decode
            |> Result.ofOption "Invalid sender: decoding"
            >>= (PublicKey.deserialize
                >> Result.ofOption "Invalid sender: deserializing")
                <@> Some)
        <@> (fun sender -> json, contractId, tx, messageBody, sender))
    <@> (fun (json, contractId, tx, messageBody, sender) ->
        contractId, json.Command, messageBody, tx, sender)

let parseAddressDBContractInfo json =
    try
        let json = ContractInfoRequestJson.Parse json
        let rlimit =
            json.Rlimit
            |> Option.map uint32

        if String.length json.Code = 0 then
            Error "Contract code is empty"
        else
            Ok (json.Code, rlimit)
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseContractActivateJson json =
    try
        let json = ContractActivateRequestJson.Parse json
        
        let rlimit =
            json.Rlimit
            |> Option.map uint32

        if String.length json.Code = 0 then
            Error "Contract code is empty"
        else if String.length json.Password = 0 then
            Error "Password is empty"
        else if json.NumberOfBlocks = 0 then
            Error "Number of blocks is zero"
        else
            Ok (json.Code, uint32 json.NumberOfBlocks, rlimit,  json.Password)
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseContractExtendJson chain json =
    try
        let json = ContractExtendRequestJson.Parse json

        if String.length json.Password = 0 then
            Error "Password is empty"
        else if json.NumberOfBlocks = 0 then
            Error "Number of blocks is zero"
        else
            match Address.decodeContract chain json.Address with
            | Error err -> Error ("Address is invalid: " + err)
            | Ok contractId -> Ok (contractId, uint32 json.NumberOfBlocks, json.Password)
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parsePublishBlockJson json =
    try
        match JsonValue.Parse json with
        | JsonValue.String block ->
            match Block.fromHex block with
            | Some block -> Ok block
            | None -> Error "invalid block"
        | JsonValue.Record record ->
            let tryFind name =
                Array.tryFind (fun (name',_) -> name' = name) record
                |> Option.bind (function
                    | _, JsonValue.String s -> Some s
                    | _ -> None)

            let block = tryFind "block"
            let header = tryFind "header"
            let body = tryFind "body"

            match block,header,body with
            | Some block,None,None ->
                match Block.fromHex block with
                | Some block -> Ok block
                | None -> Error "invalid block"
            | None, Some header, Some body ->
                if String.length header <> Block.HeaderSize * 2 then
                    Error "invalid header size"
                else
                    match Block.fromHex (header + body) with
                    | Some block -> Ok block
                    | None -> Error "invalid block"
            | _ -> Error "Json is invalid"
        | _ -> Error "Json is invalid"
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseSubmitBlockHeaderJson json =
    try
        let json = SubmitBlockHeaderJson.Parse json

        match BlockHeader.fromHex json.Header with
        | Some header -> Ok header
        | None -> Error "invalid header"
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseImportSeedJson json =
    try
        let json = ImportSeedJson.Parse json
        let words = List.ofArray json.Words

        if String.length json.Password = 0 then
            Error "Password is empty"
        else
            Ok (words, json.Password)
    with ex ->
        Error ("Json is invalid: " + ex.Message)


let parseGetPublicKeyJson json =
    try
        let json = GetPublicKeyJson.Parse json

        if String.length json.Path = 0 then
            Error "Path is empty"
        else if String.length json.Password = 0 then
            Error "Password is empty"
        else
            Ok (json.Path, json.Password)
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseSignJson json =
    try
        let json = SignJson.Parse json

        if String.length json.Path = 0 then
            Error "Path is empty"
        else if String.length json.Password = 0 then
            Error "Password is empty"
        else
            match Hash.fromString json.Message with
            | Some message ->
                Ok (message, json.Path, json.Password)
            | None -> Error "invalid message"
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseCheckPasswordJson json =
    try
        let json = CheckPasswordJson.Parse json

        if String.length json.Password = 0 then
            Error "Password is empty"
        else
            Ok json.Password
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseChangePassword json =
    try
        let json = ChangePasswordJson.Parse json

        if String.length json.Old = 0 || String.length json.New = 0 then
            Error "Password are empty"
        else
            Ok (json.Old, json.New)
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseTransactionsRequestJson query =
    match Map.tryFind "take" query, Map.tryFind "skip" query with
    | Some take, Some skip ->
        match System.Int32.TryParse take, System.Int32.TryParse skip with
        | (true,take),(true,skip) ->
            if skip < 0 || take < 0 then
                Error "Invalid values"
            else
                Ok (skip,take)
        | _ ->
            Error "Invalid values"
    | _ -> Error "Invalid values"
    
let parseHeadersRequestJson query defaultTip =
    match Map.tryFind "take" query with
    | Some take ->
        let blockNumber =
            Map.tryFind "blockNumber" query
            |> Option.map System.Int32.TryParse
            |> Option.filter fst
            |> Option.map snd
            |> Option.defaultValue defaultTip
                
        match System.Int32.TryParse take with
        | (true,take) ->
            if take < 0 then
                Error "Invalid values"
            else
                Ok (blockNumber,take)
        | _ ->
            Error "Invalid values"
    | None ->
        Error "missing query data"
let parseAddress json =
    try
        let address = AddressJson.Parse json

        address.Address |> Ok
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseTxHexJson json =
    try
        let hex = TxHexJson.Parse json

        FsBech32.Base16.decode hex.Tx
        |> Option.bind Consensus.Serialization.TransactionExtended.deserialize
        |> Infrastructure.Result.ofOption "invalid transaction"
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseRestoreNewAddress json =
    try
        let json = RestoreNewAddressesJson.Parse json
        json.Max |> Ok
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseImportZenPublicKey json =
    try
        let json = ImportZenPublicKey.Parse json
        json.PublicKey |> Ok
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let private checkAddresses chain addresses =
    if Array.isEmpty addresses then
        Error "Empty addresses list"
    else
        addresses
        |> Array.toList
        |> List.distinct
        |> Infrastructure.Result.traverseResultM (Address.decodeAny chain)
        <@> List.map (Wallet.Address.encode chain)

let parseGetBalanceJson chain json =
    try
        
        let json =
            json
            |> JsonValue.Parse
            |> GetBalanceJson.Root
        
        let blockNumber =
            json.BlockNumber
            |> Option.map uint32
        
        checkAddresses chain json.Addresses
        >>= (fun addresses -> Ok (addresses,blockNumber))
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)

let parseGetHistoryJson chain json =
    try
        let json = GetHistoryJson.Parse json

        checkAddresses chain json.Addresses
        <@> fun addresses -> addresses, json.Skip, json.Take
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)

let parseDiscoveryJson chain json =
    try
        let json = DiscoveryJson.Parse json

        checkAddresses chain json.Addresses
        <@> fun addresses ->
            match json.Full with
            | Some isFull -> (addresses,isFull)
            | None -> (addresses, false)
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)

let parseGetHistoryByBlockNumberJson chain json =
    try
        let json = GetHistoryFilterByBlockJson.Parse json

        checkAddresses chain json.Addresses
        <@> fun addresses -> addresses, uint32 json.Start, uint32 json.End
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)

let parseGetOutputsJson chain json =
    try
        let json = GetOutputsJson.Parse json

        checkAddresses chain json.Addresses
        >>= fun addresses ->
            match json.Mode with
            | "all" ->
                Ok (addresses, Messaging.Services.AddressDB.Mode.All)
            | "unspentOnly" ->
                Ok (addresses, Messaging.Services.AddressDB.Mode.UnspentOnly)
            | _ ->
                Error "unrecognized mode"
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)
let parseTransactionCountJson chain json =
    try
        let json = GetTransactionCountJson.Parse json

        checkAddresses chain json.Addresses
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)
        
let parseAsset json =
    try
        let json = GetAssetsJson.Parse json

        match Asset.fromString json.Asset with
        | Some asset -> Ok asset
        | None -> Error "invalid asset"

    with _ as ex ->
        Error ("Json invalid: " + ex.Message)

let parseGetContractHistoryJson json =
    try
        let json = GetContractHistoryJson.Parse json

        json.ContractId
        |> ContractId.fromString
        |> Result.ofOption "invalid contractId"
        <@> fun contractId -> contractId, json.Skip, json.Take
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)
        
let parseGetContractHistoryByBlockNumber json =
    try
        let json = GetContractHistoryFilterByBlockJson.Parse json

        json.ContractId
        |> ContractId.fromString
        |> Result.ofOption "invalid contractId"
        <@> fun contractId -> contractId, uint32 json.Start,uint32 json.End
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)
        
             
let handleTxResult config result =
    match result with
    | Error error ->
        config.replyError error
    | Ok tx ->
        Transaction.hash tx
        |> Hash.toString
        |> JsonValue.String
        |> JsonContent
        |> config.reply StatusCode.OK

let handleRawTxResult config result =
    match result with
    | Error error ->
        config.replyError error
    | Ok raw ->
        let txHash = Transaction.fromRaw raw |> Transaction.hash |> Hash.toString
        let hex = Serialization.RawTransaction.toHex raw

        (new RawTransactionResultJson.Root(txHash, hex)).JsonValue
        |> JsonContent
        |> config.reply StatusCode.OK


