module Node.Tests.ApiTests

open NUnit.Framework
open FsUnit
open Infrastructure
open Consensus
open Consensus.Chain
open Tests.SampleContract
open Api.Types
open Setup
    
[<OneTimeSetUp>]
let setUp = fun () ->
    clean()
    createBroker()
    initBlockchainActor()
    initWalletActor()
    initApiActor()
//    initGenesis()
//    
[<TearDown>]
clean()

[<Test>]
let ``Should get 'no wallet' for balance request``() =
    let balance = BalanceResponseJson.Load(getUri "wallet/balance")
    printfn "%A" balance
//    let response = activate.JsonValue.Request ("http://" + apiUri + "/wallet/contract/activate")
//    response.StatusCode |> should equal 200
//    let responseBody =
//        match response.Body with
//        | FSharp.Data.Text string -> string
//        | _ -> failwith "unexpected response type"
//    let response' = ContractActivateResponseJson.Parse responseBody
//    let execute = new ContractExecuteRequestJson.Root(response'.Address,"", "", [| new ContractExecuteRequestJson.Spend("", "", int64 0) |])
//    let response = execute.JsonValue.Request ("http://" + apiUri + "/wallet/contract/execute")
//    response.StatusCode |> should equal 200

[<Test>]
let ``Contract should activate and execute``() =
    let activate = new ContractActivateRequestJson.Root(sampleContractCode, 10)
    let response = activate.JsonValue.Request (apiUri + "/wallet/contract/activate")
    response.StatusCode |> should equal 200
    let responseBody =
        match response.Body with
        | FSharp.Data.Text string -> string
        | _ -> failwith "unexpected response type"
    let response' = ContractActivateResponseJson.Parse responseBody
    let execute = new ContractExecuteRequestJson.Root(response'.Address,"", "", [| new ContractExecuteRequestJson.Spend("", "", int64 0) |])
    let response = execute.JsonValue.Request (apiUri + "/wallet/contract/execute")
    response.StatusCode |> should equal 200

//[<Test>]
//let ``Contract should activate and execute``() =
//    let activate = new ContractActivateRequestJson.Root(sampleContractCode, 10)
//    let response = activate.JsonValue.Request ("http://" + apiUri + "/wallet/contract/activate")
//    response.StatusCode |> should equal 200
//    let responseBody =
//        match response.Body with
//        | FSharp.Data.Text string -> string
//        | _ -> failwith "unexpected response type"
//    let response' = ContractActivateResponseJson.Parse responseBody
//    let execute = new ContractExecuteRequestJson.Root(response'.Address,"", "", [| new ContractExecuteRequestJson.Spend("", "", int64 0) |])
//    let response = execute.JsonValue.Request ("http://" + apiUri + "/wallet/contract/execute")
//    response.StatusCode |> should equal 200
//    
//[<Test>]
//let ``Should import account``() =
//    let activate = new ContractActivateRequestJson.Root(sampleContractCode, 10)
//    let response = activate.JsonValue.Request ("http://" + apiUri + "/wallet/contract/activate")
//    response.StatusCode |> should equal 200
//    let responseBody =
//        match response.Body with
//        | FSharp.Data.Text string -> string
//        | _ -> failwith "unexpected response type"
//    let response' = ContractActivateResponseJson.Parse responseBody
//    let execute = new ContractExecuteRequestJson.Root(response'.Address,"", "", [| new ContractExecuteRequestJson.Spend("", "", int64 0) |])
//    let response = execute.JsonValue.Request ("http://" + apiUri + "/wallet/contract/execute")
//    response.StatusCode |> should equal 200