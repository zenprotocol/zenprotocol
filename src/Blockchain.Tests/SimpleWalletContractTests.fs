module Blockchain.Tests.SimpleWalletContractTests

open NUnit.Framework
open FsUnit
open Blockchain
open Consensus
open Consensus.Types
open Wallet
open Messaging.Services.Blockchain
open Messaging.Events
open Infrastructure
open Consensus.Tests.ContractTests
open Blockchain.State
open Consensus.Tests

open TestsInfrastructure.Constraints
open Consensus.Tests.SampleContract
open Helper

let chain = Chain.getChainParameters Chain.Local

let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction (fun _ -> UtxoSet.NoOutput) rootTxHash rootTx
let mempool = MemPool.empty |> MemPool.add rootTxHash rootTx
let orphanPool = OrphanPool.create()
let acs = ActiveContractSet.empty

let mutable state = {
    memoryState =
        {
            utxoSet = utxoSet
            mempool = mempool
            orphanPool = orphanPool
            activeContractSet = acs
            contractCache = ContractCache.empty
        }
    tipState =
        {
            tip = ExtendedBlockHeader.empty
            activeContractSet = acs
            ema=EMA.create chain
        }
    initialBlockDownload = InitialBlockDownload.Inactive
    headers=0ul
}

let account = createTestAccount()

let shouldBeOk result =
    result
    |> Result.mapError failwith
    |> ignore

let shouldBeErrorMessage message =
    function
    | Ok _ -> failwithf "Expected '%A' error message, got ok" message
    | Error err -> err |> should equal message

let activateContract code account session state =
    Account.createActivateContractTransaction chain code 1ul account
    |> Result.map (fun tx ->
        let events, state =
            Handler.handleCommand chain (ValidateTransaction tx) session 1UL state
            |> Writer.unwrap
        let txHash = Transaction.hash tx
        events |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (txHash, tx)))
        let contractId = Contract.makeContractId Version0 code
        ActiveContractSet.containsContract contractId state.memoryState.activeContractSet
        |> should equal true
        (state, contractId)
    )

let dataPath =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]
let databaseContext = DatabaseContext.createTemporary dataPath
let session = DatabaseContext.createSession databaseContext

let mutable contractId = ContractId (Version0,Hash.zero)

let clean() =
    Platform.cleanDirectory dataPath

[<OneTimeSetUp>]
let setUp = fun () ->
    clean()
    activateContract """
    open Zen.Types
    open Zen.Util
    open Zen.Base
    open Zen.Cost
    open Zen.Asset

    module W = Zen.Wallet
    module RT = Zen.ResultT
    module Tx = Zen.TxSkeleton

    let main txSkeleton _ contractHash command sender data wallet =
        let! result =
            Tx.lockToPubKey zenAsset 10UL (hashFromBase64 "DYggLLPq6eXj1YxjiPQ5dSvb/YVqAVNf8Mjnpc9P9BI=") txSkeleton
            >>= Tx.fromWallet zenAsset 10UL contractHash wallet in

        let result' =
            match result with
            | Some tx -> Some (tx, None)
            | None -> None in

        RT.of_option "not enough Zens" result'

    val cf: txSkeleton -> context -> string -> sender -> option data -> wallet -> cost nat 12
        let cf _ _ _ _ _ wallet =
            let res : nat = (64 + (W.size wallet * 128 + 192) + 0 + 21) in
            ret res
    """ account session state
    |> function
    | Ok (state', cHash') ->
        contractId <- cHash'
        state <- state'
    | Error error ->
        failwith error

[<TearDown>]
let tearDown = fun () ->
    clean()

[<Test>]
let ``Wallet using contract should execute``() =
    let output = {lock=Contract contractId;spend={asset=Asset.Zen;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} (UtxoSet.Unspent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx 1ul 1_000_000UL contractId "" None None { state.memoryState with utxoSet = utxoSet }
    |> shouldBeOk

[<Test>]
let ``Contract should not be able to lock more token than available``() =
    let output = {lock=Contract contractId;spend={asset=Asset.Zen;amount=9UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=1ul} (UtxoSet.Unspent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx 1ul 1_000_000UL contractId "" None None { state.memoryState with utxoSet = utxoSet }
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output is missing``() =

    TransactionHandler.executeContract session sampleInputTx 1ul 1_000_000UL contractId "" None None state.memoryState
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output locked to PK address``() =
    let output = {lock=PK Hash.zero;spend={asset=Asset.Zen;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} (UtxoSet.Unspent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx 1ul 1_000_000UL contractId "" None None { state.memoryState with utxoSet = utxoSet }
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output is spent``() =
    let output = {lock=PK Hash.zero;spend={asset=Asset.Zen;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} (UtxoSet.Spent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx 1ul 1_000_000UL contractId "" None None { state.memoryState with utxoSet = utxoSet }
    |> shouldBeErrorMessage "not enough Zens"
