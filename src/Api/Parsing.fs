module Api.Parsing

open Api
open FSharp.Data
open Api.Types
open Consensus
open Consensus.Types
open FsBech32
open System.Text
open Newtonsoft.Json

let private getSpend' asset assetType amount =
    match Hash.fromString asset, Hash.fromString assetType with
    | Ok asset, Ok assetType ->
        Ok { asset = asset, assetType; amount = uint64 amount }
    | Error msg, Ok _ ->
        Error <| sprintf "Asset: %s" msg
    | Ok _, Error msg ->
        Error <| sprintf "AssetType: %s" msg
    | Error msg1, Error msg2->
        Error <| sprintf "Asset: %s, AssetType %s" msg1 msg2

let getSpend chain json =
    try
        let json = SpendRequestJson.Parse json

        Address.decodePK chain json.Address
        |> function
        | Error err ->
            Error ("Address is invalid: " + err)
        | Ok pkHash ->
            let s = json.Spend
            getSpend' s.Asset s.AssetType s.Amount
            |> Result.map (fun spend -> (pkHash, spend))
    with _ as ex ->
        Error ("Json invalid: " + ex.Message)

let getContractExecute chain json =
    try
        let json = ContractExecuteRequestJson.Parse json
        match Address.decodeContract chain json.Address with
        | Error err -> Error ("Address is invalid: " + err)
        | Ok cHash ->
            let mutable spends = Map.empty
            let mutable errors = List.empty

            for item in json.Spends do
                getSpend' item.Asset item.AssetType item.Amount
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

                Ok (cHash, json.Command, data, json.Options.ReturnAddress, sign, spends)
            else
                errors
                |> String.concat " "
                |> Error
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let getContractActivate json =
    try
        let json = ContractActivateRequestJson.Parse json

        if String.length json.Code = 0 then
            Error "Contract code is empty"
        else
            Ok (json.Code, uint32 json.NumberOfBlocks)
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let getPublishBlock json =
    try
        let json = PublishBlockJson.Parse json

        match Block.fromHex json.Block with
        | Some block -> Ok block
        | None -> Error "invalid block"
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let getImportSeed json =
    try
        let json = ImportSeedJson.Parse json

        let mutable words = List.empty

        for item in json.Words do
            words <- item :: words

        let key = Encoding.ASCII.GetBytes json.Key
        Ok (List.rev words, key)
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let getUnlock json =
    try
        let json = UnlockAccountJson.Parse json

        Encoding.ASCII.GetBytes json.Key
        |> Ok
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)
