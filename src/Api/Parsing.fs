module Api.Parsing

open FSharp.Data
open Api.Types
open Consensus
open Consensus.Types

let private getSpend' asset amount =
    match Hash.fromString asset with
    | Error err -> Error ("Asset invalid: " + err)
    | Ok asset -> Ok { asset = asset; amount = uint64 amount }

let getSpend chain json =
    try
        let json = SpendRequestJson.Parse json

        Address.decodePK chain json.Address
        |> function 
        | Error err -> 
            Error ("Address is invalid: " + err)
        | Ok pkHash ->
            getSpend' json.Spend.Asset json.Spend.Amount
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
                Ok (cHash, json.Command, spends)
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