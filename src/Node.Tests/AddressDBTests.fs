module Node.Tests.AddressDB

open NUnit.Framework
open FsUnit
open Consensus
open Infrastructure
open FSharp.Data
open Consensus.Tests.Helper
open Consensus.Tests.SampleContract
open Api.Types
open Utils
open Config
open Json
open FsBech32
open Serialization

let password = "1234"

[<SetUp>]
[<TearDown>]
let clean() =
    Platform.cleanDirectory dataPath

let i = ignore

[<Test>]
let ``AddressDB should show contract history``() =
    use actors = getActors()

    Block.createGenesis chainParams [rootTxExtended] (0UL,0UL)
    |> publishBlockJson 
    |> post "blockchain/publishblock" |> i

    importSeedJson password (split rootMnemonicPhrase [|' '|])
    |> post "wallet/import" |> i
    
    "wallet/resync"
    |> get |> i

    let response = 
        contractActivateRequestJson sampleContractCode 10 password
        |> post "wallet/contract/activate"
        |> ContractActivateResponseJson.Parse

    [ ] //spend
    |> contractExecuteRequestJson response.Address "mock command" "" true "" password 
    |> post "wallet/contract/execute"
    |> ignore

    let contractId =
        Wallet.Address.decodeContract chain response.Address
        |> Result.get

    getContractHistoryJson contractId 0 100
    |> post "addressdb/contract/history"
    |> JsonValue.Parse
    |> JsonExtensions.AsArray
    |> parseContractCommandHistoryResultJson
    |> Array.map (fun (fst, _, _) -> fst)
    |> should contain "mock command"