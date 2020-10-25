module Node.Tests.ContractTests

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
let ``Wallet should import and show balance``() =
    use actors = getActors()

    Block.createGenesis chainParams [rootTxExtended] (0UL,0UL)
    |> publishBlockJson 
    |> post "blockchain/publishblock" |> i
    
    importSeedJson password (split rootMnemonicPhrase [|' '|])
    |> post "wallet/import" |> i
    
    "wallet/resync"
    |> get |> i

    "wallet/balance"
    |> get
    |> BalanceResponseJson.Parse
    |> parseBalanceJson
    |> should contain (Asset.Zen, rootAmount) 
    
[<Test>]
let ``Contract should activate and execute``() =
    use actors = getActors()

    Block.createGenesis chainParams [rootTxExtended] (0UL,0UL)
    |> publishBlockJson 
    |> post "blockchain/publishblock" |> i

    importSeedJson password (split rootMnemonicPhrase [|' '|])
    |> post "wallet/import" |> i
    
    "wallet/resync"
    |> get |> i

    let response = 
        contractActivateRequestJson sampleContractCode 10 2723280 password
        |> post "wallet/contract/activate"
        |> ContractActivateOrExtendResponseJson.Parse

    [ ] //spend
    |> contractExecuteRequestJson response.Address "" "" true "" password 
    |> post "wallet/contract/execute"
    |> printfn "txHash: %A" //TODO: validate

[<Test>]
let ``Contract should activate and execute (from transaction)``() =
    use actors = getActors()

    Block.createGenesis chainParams [rootTxExtended] (0UL,0UL)
    |> publishBlockJson 
    |> post "blockchain/publishblock" |> i

    importSeedJson password (split rootMnemonicPhrase [|' '|])
    |> post "wallet/import" |> i
    
    "wallet/resync"
    |> get |> i
    
    let response = 
        contractActivateRequestJson sampleContractCode 10 2723280 password
        |> post "wallet/contract/activate"
        |> ContractActivateOrExtendResponseJson.Parse

    TxSkeleton.empty
    |> TxSkeleton.serialize
    |> Base16.encode
    |> contractExecuteFromTransactionJson response.Address "" "" ""
    |> post "blockchain/contract/execute"
    |> Base16.decode
    |> Option.get
    |> Transaction.deserialize TransactionSerializationMode.Full
    |> printfn "tx: %A" //TODO: validate
