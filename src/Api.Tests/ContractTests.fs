module Api.Tests.ContractTests

open NUnit.Framework
open FsUnit
open Consensus
open Infrastructure
open Consensus.Tests.Helper
open Consensus.Tests.SampleContract
open Api.Types
open Utils
open Json
open FsBech32
open Serialization
open Constants

[<SetUp>]
[<TearDown>]
let clean() =
    Platform.cleanDirectory dataPath

let i = ignore
#if DEBUG
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
    |> Option.map Transaction.hash
    |> Option.map Hash.toString
    |> Option.defaultValue "should not be None"
    |> should equal "d5c5663e704f1c68b9bd3137c5a51222f5946d615d91d82190d74af7deeb6ac9"
#endif