module Api.Parsing

open Api
open FSharp.Data
open Api.Types
open Consensus
open Consensus.Types
open FsBech32
open System.Text
open Newtonsoft.Json

let private getSpend asset assetType amount =
    match Hash.fromString asset, Hash.fromString assetType with
    | Ok asset, Ok assetType ->
        Ok { asset = asset, assetType; amount = uint64 amount }
    | Error msg, Ok _ ->
        Error <| sprintf "Asset: %s" msg
    | Ok _, Error msg ->
        Error <| sprintf "AssetType: %s" msg
    | Error msg1, Error msg2->
        Error <| sprintf "Asset: %s, AssetType %s" msg1 msg2

let parseSendJson chain json =
    try
        let json = SendRequestJson.Parse json

        if String.length json.Password = 0 then
            Error "Password is empty"
        else
            Address.decodePK chain json.Address
            |> function
            | Error err ->
                Error ("Address is invalid: " + err)
            | Ok pkHash ->
                let s = json.Spend
                getSpend s.Asset s.AssetType s.Amount
                |> Result.map (fun spend -> (pkHash, spend, json.Password))
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
            | Ok cHash ->
                let mutable spends = Map.empty
                let mutable errors = List.empty
    
                for item in json.Spends do
                    getSpend item.Asset item.AssetType item.Amount
                    |> function
                    | Ok spend -> spends <- Map.add spend.asset spend.amount spends
                    | Error err -> errors <- err :: errors
                    |> ignore
    
                if List.isEmpty errors then
                    let data =
                        if System.String.IsNullOrEmpty json.Data then
                            None
                        else
                            match Base16.decode json.Data with
                            | Some data ->
                                match Serialization.Data.deserialize data with
                                | Some data -> Some data
                                | None -> failwith "Invalid Data"
                            | None -> failwith "Invalid Data"
    
                    let sign =
                        if System.String.IsNullOrEmpty json.Options.Sign then
                            None
                        else
                            Some json.Options.Sign
    
                    Ok (cHash, json.Command, data, json.Options.ReturnAddress, sign, spends, json.Password)
                else
                    errors
                    |> String.concat " "
                    |> Error
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseContractActivateJson json =
    try
        let json = ContractActivateRequestJson.Parse json

        if String.length json.Code = 0 then
            Error "Contract code is empty"
        else if String.length json.Password = 0 then
            Error "Password is empty"
        else
            Ok (json.Code, uint32 json.NumberOfBlocks, json.Password)
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parsePublishBlockJson json =
    try
        let json = PublishBlockJson.Parse json

        match Block.fromHex json.Block with
        | Some block -> Ok block
        | None -> Error "invalid block"
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let parseImportSeedJson json =
    try
        let json = ImportSeedJson.Parse json

        let mutable words = List.empty

        for item in json.Words do
            words <- item :: words

        if String.length json.Password = 0 then
            Error "Password is empty"
        else
            Ok (List.rev words, json.Password)
    with _ as ex ->
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

let parseCheckPasswordJson json =
    try
        let json = CheckPasswordJson.Parse json

        if String.length json.Password = 0 then
            Error "Password is empty"
        else
            Ok json.Password
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)