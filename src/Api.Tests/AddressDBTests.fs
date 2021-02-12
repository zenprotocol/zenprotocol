module Api.Tests.AddressDB

open NUnit.Framework
open FsUnit
open Consensus
open Infrastructure
open FSharp.Data
open Consensus.Tests.Helper
open Api.Types
open Utils
open Json
open Consensus.Tests.SampleContract
open Serialization
open Config
open Http

[<SetUp>]
[<TearDown>]
let clean() =
    Platform.cleanDirectory dataPath

let i = ignore
#if DEBUG
[<Test>]
let ``AddressDB should show contract history``() =
    use actors = getActors()

    Block.createGenesis chainParams [rootTxExtended] (0UL,0UL)
    |> publishBlockJson 
    |> post "blockchain/publishblock" |> i

    importSeedJson Constants.password (split rootMnemonicPhrase [|' '|])
    |> post "wallet/import" |> i
    
    "wallet/resync"
    |> get |> i

    let response = 
        contractActivateRequestJson sampleContractCode 10 2723280 Constants.password
        |> post "wallet/contract/activate"
        |> ContractActivateOrExtendResponseJson.Parse

    [ ] //spend
    |> contractExecuteRequestJson response.Address "mock command" "" true "" Constants.password 
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
#endif
#if DEBUG    
[<Test>]
let ``should get contract history`` () =
    let chainParams = { Utils.chainParams with genesisHashHash = Hash.fromString "46d1e8e4d397bbcf13f96346dfb998d7445600806d09e6e5d1740b3d8412fd24" |> Option.get |> Hash.computeOfHash }
    use actors = customChainActor chainParams
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client; chain = Chain.getChain chainParams }
    let contentConfig e = contentConfig e |> fun conf -> { conf with client = client; chain = Chain.getChain chainParams }
    
    let body = Constants.publishBlock

    Api.Server.Blockchain.publishBlock config body
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    Api.Server.Wallet.resync config
    
    let body = Constants.activateContract
    Api.Server.Wallet.Contract.activate config body
    
    let body = Constants.executeContract

    Api.Server.Wallet.Contract.execute config body
    
    Api.Server.Blockchain.info config
    
    let expectedContent =
        Constants.contractExecute
        |> Api.Types.ContractCommandHistoryResultJson.Parse
        |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent
    let body = "{\"contractId\": \"0000000060de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3\", \"skip\": 0, \"take\": 10}"

    Api.Server.AddressDB.contractHistory (contentConfig expectedContent) body
#endif
#if DEBUG
[<Test>]
let ``should get contract mint`` () =
    let chainParams = { Utils.chainParams with genesisHashHash = Hash.fromString "46d1e8e4d397bbcf13f96346dfb998d7445600806d09e6e5d1740b3d8412fd24" |> Option.get |> Hash.computeOfHash }
    use actors = customChainActor chainParams
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client; chain = Chain.getChain chainParams }
    let contentConfig e = contentConfig e |> fun conf -> { conf with client = client; chain = Chain.getChain chainParams }
    
    let body = Constants.publishBlock

    Api.Server.Blockchain.publishBlock config body
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    Api.Server.Wallet.resync config
    
    let body = Constants.activateContract
    Api.Server.Wallet.Contract.activate config body
    
    let body = Constants.executeContract

    Api.Server.Wallet.Contract.execute config body
    let body = Constants.publishSecondBlock

    Api.Server.Blockchain.publishBlock config body
    
    Api.Server.Blockchain.info config
    
    let expectedContent = JsonContent <| JsonValue.Parse Constants.contractMint
    let body = "{\"asset\": \"0000000060de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3\"}"

    Api.Server.AddressDB.contractMint (contentConfig expectedContent) body
#endif
#if DEBUG
[<Test>]
let ``Should get transactions``() =
    let chainParams = { Utils.chainParams with genesisHashHash = Hash.fromString "46d1e8e4d397bbcf13f96346dfb998d7445600806d09e6e5d1740b3d8412fd24" |> Option.get |> Hash.computeOfHash }
    use actors = customChainActor chainParams
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client; chain = Chain.getChain chainParams }
    
    let body = Constants.publishBlock

    Api.Server.Blockchain.publishBlock config body
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    Api.Server.Wallet.resync config
        
    let expectedContent =
        Constants.oneTransactionResponse
        |> Api.Types.TransactionsResponseJson.Parse
        |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent
    let body = Constants.transactionSkipTakeOneAddress

    Api.Server.AddressDB.transactions (debugConfig expectedContent) body
    let expectedContent = JsonContent <| JsonValue.String "03ae57cd8ee6339ad138bf2c1c7dae7b3571d839bc987a818d8c067a7801a784"
    let body = Constants.sendTo
   
    Api.Server.Wallet.send (contentConfig expectedContent) body
 
    Api.Server.Blockchain.info config
    Api.Server.Blockchain.info config
    
    let expectedContent =
        Constants.twoTransactionResponse
        |> Api.Types.TransactionsResponseJson.Parse
        |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent
    let body = Constants.transactionSkipTakeTwoAddress

    Api.Server.AddressDB.transactions (debugConfig expectedContent) body
#endif
#if DEBUG
[<Test>]
let ``Should get contract info`` () =
    let chainParams = { Utils.chainParams with genesisHashHash = Hash.fromString "46d1e8e4d397bbcf13f96346dfb998d7445600806d09e6e5d1740b3d8412fd24" |> Option.get |> Hash.computeOfHash }
    use actors = customChainActor chainParams
    let client = ServiceBus.Client.create Utils.busName
    let contentConfig e = {contentConfig e with client = client; chain = Chain.getChain chainParams }
    let body = Constants.contractInfo
    let expectedContent = JsonContent <| JsonValue.Parse Constants.contractInfoResponse
    Api.Server.AddressDB.contractInfo (contentConfig expectedContent) body
#endif
#if DEBUG
[<Test>]
let ``Should get outputs``() =
    let chainParams = { Utils.chainParams with genesisHashHash = Hash.fromString "46d1e8e4d397bbcf13f96346dfb998d7445600806d09e6e5d1740b3d8412fd24" |> Option.get |> Hash.computeOfHash }
    use actors = customChainActor chainParams
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client; chain = Chain.getChain chainParams }
    
    let body = Constants.publishBlock

    Api.Server.Blockchain.publishBlock config body
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    Api.Server.Wallet.resync config
        
    let expectedContent =
        Constants.oneOutputResponse
        |> Api.Types.TransactionsResponseJson.Parse
        |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent
    let body = Constants.addressDBOutputs

    Api.Server.AddressDB.outputs (debugConfig expectedContent) body


    let body = Constants.sendTo
   
    Api.Server.Wallet.send config body
    
    Api.Server.Blockchain.info config
    Api.Server.Blockchain.info config
        
    let expectedContent =
        Constants.oneOutpointAfterSend
        |> Api.Types.TransactionsResponseJson.Parse
        |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent
    let body = Constants.addressDBOutputs

    Api.Server.AddressDB.outputs (debugConfig expectedContent) body
#endif


[<Test>]
let ``Should get some balance``() =
    let chainParams = { Utils.chainParams with genesisHashHash = Hash.fromString "46d1e8e4d397bbcf13f96346dfb998d7445600806d09e6e5d1740b3d8412fd24" |> Option.get |> Hash.computeOfHash }
    use actors = customChainActor chainParams
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client; chain = Chain.getChain chainParams }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client; chain = Chain.getChain chainParams }
    
    let body = Constants.publishBlock

    Api.Server.Blockchain.publishBlock config body
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    Api.Server.Wallet.resync config
    
    
    let expectedContent =
        Api.Types.BalanceResponseJson.Parse ("[{\"asset\": \"00\",\"balance\": 2300000000000}]") |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent
    let body = Constants.transactionOneAddress

    Api.Server.AddressDB.balance (contentConfig expectedContent) body


#if DEBUG
[<Test>]    
let ``Should get transaction count``() =
    let chainParams = { Utils.chainParams with genesisHashHash = Hash.fromString "46d1e8e4d397bbcf13f96346dfb998d7445600806d09e6e5d1740b3d8412fd24" |> Option.get |> Hash.computeOfHash }
    use actors = customChainActor chainParams
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client; chain = Chain.getChain chainParams }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client; chain = Chain.getChain chainParams }
    
    let body = Constants.publishBlock

    Api.Server.Blockchain.publishBlock config body
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    Api.Server.Wallet.resync config
    
    let expectedContent = JsonContent <| JsonValue.Number 1M
    let body = Constants.transactionOneAddress

    Api.Server.AddressDB.transactionCount (contentConfig expectedContent) body
    
    let body = Constants.sendTo
   
    Api.Server.Wallet.send config body
    
    //we need a request in between to make sure the addressdb actor computed the right data
    Api.Server.Blockchain.info config
        
    let expectedContent = JsonContent <| JsonValue.Number 2M
    let body = Constants.transactionTwoAddress
    Api.Server.AddressDB.transactionCount (contentConfig expectedContent) body
#endif
