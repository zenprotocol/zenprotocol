module Api.Tests.Wallet

open Consensus
open NUnit.Framework
open FSharp.Data
open Consensus.Tests.Helper
open Infrastructure
open Utils
open Config
open Infrastructure.Http

[<SetUp>]
[<TearDown>]
let clean() =
    Platform.cleanDirectory dataPath

let i = ignore

[<Test>]
let ``Should change password ``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client }
    
    let body = Constants.importWithoutPassField
    
    Api.Server.Wallet.import config body
    
    let expectedContent = JsonContent <| JsonValue.String "Password changed"
    let body = Constants.changePassword
    
    Api.Server.Wallet.changePassword (contentConfig expectedContent) body
    
    let expectedContent = JsonContent <| JsonValue.Boolean true
    let body = Constants.justPassword
    
    Api.Server.Wallet.checkPassword (contentConfig expectedContent) body

[<Test>]
let ``Should check password ``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client }
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    let expectedContent = JsonContent <| JsonValue.Boolean true
    let body = Constants.justPassword
    
    Api.Server.Wallet.checkPassword (contentConfig expectedContent) body

[<Test>]
let ``Should check existance``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client }
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    let expectedContent = JsonContent <| JsonValue.Boolean true
    
    Api.Server.Wallet.exist (contentConfig expectedContent)
    
[<Test>]
let ``Should import mnemonicPhrase``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client }
    
    let expectedContent = TextContent "account imported"
    let body = Constants.import
    
    Api.Server.Wallet.import (contentConfig expectedContent) body
    
    let expectedContent = TextContent rootMnemonicPhrase
    let body = Constants.justPassword
    
    Api.Server.Wallet.mnemonicphrase (contentConfig expectedContent) body

[<Test>]
let ``Should get public key``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client }
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    let expectedContent = JsonContent <| JsonValue.String "02b43a1cb4cb6472e1fcd71b237eb9c1378335cd200dd07536594348d9e450967e"
    let body = Constants.publicKeyRecord
    
    Api.Server.Wallet.publicKey (contentConfig expectedContent) body


[<Test>]
let ``Should get Zen public key``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client }
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    let expectedContent = JsonContent <| JsonValue.String "tpubDCeTZv9MDcNe6Ahv8UQaoAWhK9XKmpzzJjpiVbCZ4jhsJYdN67Qh18nDjuJFtWfGfLL2hRkGid6Ga5h2FoW9QoRjdcEUQRBW4tkpkCbMtKb"
    
    Api.Server.Wallet.zenPublicKey (contentConfig expectedContent)
[<Test>]
let ``Should remove and import wallet``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client }
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    let expectedContent = TextContent "wallet removed"
    let body = Constants.justPassword
    
    Api.Server.Wallet.remove (contentConfig expectedContent) body
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    let expectedContent = TextContent rootMnemonicPhrase
    let body = Constants.justPassword
    
    Api.Server.Wallet.mnemonicphrase (contentConfig expectedContent) body
[<Test>]
let ``Should fail reimport Zen public key``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    let expectedContent = TextContent "account already exist"
    let body = Constants.zenPublicKey

    Api.Server.Wallet.importZenPublicKey (badRequestConfig expectedContent) body
    
//[<Test>]
//let ``Should fail sending from imported Zen public key``() =
//    use actors = getBasicActors()
//    let client = ServiceBus.Client.create Utils.busName
//    let config = {config with client = client }
//    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client }
//    
//    let expectedContent = TextContent "zenKey imported"
//    let body = sprintf "{\"publicKey\": \"tpubDCeTZv9MDcNe6Ahv8UQaoAWhK9XKmpzzJjpiVbCZ4jhsJYdN67Qh18nDjuJFtWfGfLL2hRkGid6Ga5h2FoW9QoRjdcEUQRBW4tkpkCbMtKb\"}"
//
//    Api.Server.Wallet.importZenPublicKey (contentConfig expectedContent) body
//
//    let expectedContent = JsonContent <| JsonValue.String "03ae57cd8ee6339ad138bf2c1c7dae7b3571d839bc987a818d8c067a7801a784"
//    let body = sprintf "{\"outputs\": [{\"address\": \"tzn1q9v8sc0js2s77546lxvxcdg7aqx3ee97r2gjrx5snnduelafncw7sz5hhpf\",\"asset\": \"00\", \"amount\": \"1\"}], \"password\": %A}" password
//   
//    Api.Server.Wallet.send (contentConfig expectedContent) body
//
//[<Test>]
//let ``Should import view only address``() =
//    use actors = getBasicActors()
//    
//    ()
//    

[<Test>]
let ``Should sign a message``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client }
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    let expectedContent = JsonContent <| JsonValue.String "26f56caf7ae4b4ec60660a8bede29c9df2d8fcf464fc7307327ad30bba8cf1e8584db6a995eff5fdcb6df3edbe64d123cd931e0f33a91fdd2e16e0a327943022"
    let body = Constants.signMessage
    
    Api.Server.Wallet.sign (contentConfig expectedContent) body

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
    
    Api.Server.Wallet.balance (contentConfig expectedContent)

[<Test>]
let ``Should send some token``() =
    let chainParams = { Utils.chainParams with genesisHashHash = Hash.fromString "46d1e8e4d397bbcf13f96346dfb998d7445600806d09e6e5d1740b3d8412fd24" |> Option.get |> Hash.computeOfHash }
    use actors = customChainActor chainParams
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client; chain = Chain.getChain chainParams }
    
    let body = Constants.publishBlock

    Api.Server.Blockchain.publishBlock config body
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    Api.Server.Wallet.resync config
    
    let expectedContent = JsonContent <| JsonValue.String "03ae57cd8ee6339ad138bf2c1c7dae7b3571d839bc987a818d8c067a7801a784"
    let body = Constants.sendTo
   
    Api.Server.Wallet.send (contentConfig expectedContent) body

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
        
    let query =
        Map.empty
        |> Map.add "take" "1"
        |> Map.add "skip" "0"
        
    let body =
        Constants.oneTransactionResponse
        |> Api.Types.TransactionsResponseJson.Parse
        |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent

    Api.Server.Wallet.transactions (contentConfig body) query
    let expectedContent = JsonContent <| JsonValue.String "03ae57cd8ee6339ad138bf2c1c7dae7b3571d839bc987a818d8c067a7801a784"
    let body = Constants.sendTo
   
    Api.Server.Wallet.send (contentConfig expectedContent) body
    
    let query =
        Map.empty
        |> Map.add "take" "2"
        |> Map.add "skip" "0"
        
    let body =
        Constants.twoTransactionResponse
        |> Api.Types.TransactionsResponseJson.Parse
        |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent

    Api.Server.Wallet.transactions (contentConfig body) query

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
        
    let body =
        Constants.oneOutputResponse
        |> Api.Types.TransactionsResponseJson.Parse
        |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent

    Api.Server.Wallet.utxo (contentConfig body)
    let expectedContent = JsonContent <| JsonValue.String "03ae57cd8ee6339ad138bf2c1c7dae7b3571d839bc987a818d8c067a7801a784"
    let body = Constants.sendTo
   
    Api.Server.Wallet.send (contentConfig expectedContent) body
    
    let body =
        Constants.twoOutputResponse
        |> Api.Types.TransactionsResponseJson.Parse
        |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent

    Api.Server.Wallet.utxo (contentConfig body)

[<Test>]
let ``Should create a transaction``() =
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
    
    let expectedContent = TextContent "000000000101835300081736d721821c6316bd8f2324ce973e02ab3e579e1230c30bd5f02f17000202202b0f0c3e50543dea575f330d86a3dd01a39c97c352243352139b799ff533c3bd00000102209b95835b9af67bdc345c0ae879b93a58d0f3f90784c982a2da52fbd71d8700a3007e00021782aed7ff000101620102b43a1cb4cb6472e1fcd71b237eb9c1378335cd200dd07536594348d9e450967e0b6be83f6eb9f27d76020f1805d08aac2a3f38860177ff5ebd2cb32081094d044474e9636c3ee6c5b599ef9e93268d5d80c4abf1d4625e8e95c0ed5d6741d9b1"
    let body = Constants.sendTo

       
    Api.Server.Wallet.createTransaction (contentConfig expectedContent) body

[<Test>]
let ``Should activate a contract``() =
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
        Api.Types.ContractActivateOrExtendResponseJson.Parse ("{\"address\": \"ctzn1qqqqqqqrqm6z6y9y9p0mpjtnugst0hn908n7vp2f8m44hwznsluydyszx6vjk2kpw\",\"contractId\": \"0000000060de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3\",\"txHash\": \"0814631952dd143e9771958ed69bbff300ca6b866077d819a621fdd342ff8a2c\",\"numberOfBlocks\": \"10\"}")
        |> fun x -> x.JsonValue
        |> JsonContent
    let body = Constants.activateContract

    Api.Server.Wallet.Contract.activate (contentConfig expectedContent) body
    
    
    
[<Test>]
let ``Should execute a contract``() =
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
    
    let body = Constants.activateContract
    Api.Server.Wallet.Contract.activate config body
    
    let expectedContent = JsonContent <| JsonValue.String "42cf3f7cac8baa63f620d770a82e03cef9fd877da450e8b6b47cf267d3ad732a"
    let body = Constants.executeContract
   
    Api.Server.Wallet.Contract.execute (contentConfig expectedContent) body

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
    
    Api.Server.Wallet.transactionCount (contentConfig expectedContent)
    
    let body = Constants.sendTo
   
    Api.Server.Wallet.send config body

    let expectedContent = JsonContent <| JsonValue.Number 2M
    
    Api.Server.Wallet.transactionCount (contentConfig expectedContent)

[<Test>]
let ``Should get keys``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    let contentConfig e = contentConfig e |> fun conf -> {conf with client = client }
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    let expectedContent =
        Constants.walletKeys
        |> Api.Types.PublicKeyDataJson.Parse
        |> Array.map (fun x -> x.JsonValue)
        |> JsonValue.Array
        |> JsonContent
    let body = Constants.justPassword
 
    Api.Server.Wallet.keys (contentConfig expectedContent) body

[<Test>]
let ``Should get external address``() =
    use actors = getBasicActors()
    let client = ServiceBus.Client.create Utils.busName
    let config = {config with client = client }
    
    let body = Constants.import
    
    Api.Server.Wallet.import config body
    
    let expectedContent =
        Constants.address
        |> JsonValue.String
        |> JsonContent
 
    Api.Server.Wallet.address (contentConfig expectedContent) 
