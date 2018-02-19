module Api.Parsing

open FSharp.Data
open Api.Types
open Consensus
open Consensus.Types
open FsBech32
open System.Text

let private getSpend' asset amount =
    match Base16.decode asset with
    | None ->
        Error "Could not decode asset"
    | Some decoded when Array.length decoded = 64 ->
        match Hash.fromBytes decoded.[0..31], Hash.fromBytes decoded.[31..63] with
        | Some assetContract, Some assetToken ->
            Ok { asset = assetContract, assetToken; amount = uint64 amount }
        | _ ->
            Error ("Invalid asset hash length")
    | _ ->
        Error "Invalid asset data length"

let getSpend chain json =
    try
        let json = SpendRequestJson.Parse json

        Address.decodePK chain json.Address
        |> function
        | Error err ->
            Error ("Address is invalid: " + err)
        | Ok pkHash ->
            let s = json.Spend
            getSpend' s.Asset s.Amount
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
                getSpend' item.Asset item.Amount
                |> function
                | Ok spend -> spends <- Map.add spend.asset spend.amount spends
                | Error err -> errors <- err :: errors
                |> ignore

            if List.isEmpty errors then
                Ok (cHash, json.Command, json.Data |> Encoding.ASCII.GetBytes |> Data , spends)
            else
                errors
                |> String.concat " "
                |> Error
    with _ as ex ->
        Error ("Json is invalid: " + ex.Message)

let getContractActivate chain json =
    try
        let json = ContractActivateRequestJson.Parse json

        if String.length json.Code = 0 then
            Error "Contract code is empty"
        else
            Ok json.Code
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