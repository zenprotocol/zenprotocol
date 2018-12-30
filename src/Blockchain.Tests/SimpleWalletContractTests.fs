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
let mempool = MemPool.empty |> MemPool.add rootTxExtended
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
            contractStates = ContractStates.asDatabase
            invalidTxHashes = Set.empty
        }
    tipState =
        {
            tip = ExtendedBlockHeader.empty
            activeContractSet = acs
            ema = EMA.create chain
            cgp = CGP.empty
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
    TestWallet.createActivateContractTransaction chain code 1ul account
    |> Result.map (fun tx ->
        let events, state =
            Handler.handleCommand chain (ValidateTransaction (Transaction.toExtended tx)) session 1UL state
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
    module C = Zen.Cost

    let main txSkeleton _ contractId command sender messageBody wallet state =
        let! result =
            Tx.lockToPubKey zenAsset 10UL zeroHash txSkeleton
            >>= Tx.fromWallet zenAsset 10UL contractId wallet in

        let result' =
            match result with
            | Some tx -> Some @ { tx = tx; message = None; state = NoChange}
            | None -> None in

        RT.ofOption "not enough Zens" result'

    let cf _ _ _ _ _ wallet _ =
        C.ret (64 + (W.size wallet * 128 + 192) + 0 + 22 <: nat)
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

    let state = { state with memoryState = { state.memoryState with utxoSet = utxoSet } }

    TransactionHandler.executeContract session sampleInputTx 1_000_000UL contractId "" None None state false
    |> shouldBeOk

[<Test>]
let ``Contract should not be able to lock more token than available``() =
    let output = {lock=Contract contractId;spend={asset=Asset.Zen;amount=9UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=1ul} (UtxoSet.Unspent output) utxoSet

    let state = { state with memoryState = { state.memoryState with utxoSet = utxoSet } }

    TransactionHandler.executeContract session sampleInputTx 1_000_000UL contractId "" None None state false
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output is missing``() =

    TransactionHandler.executeContract session sampleInputTx 1_000_000UL contractId "" None None state false
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output locked to PK address``() =
    let output = {lock=PK Hash.zero;spend={asset=Asset.Zen;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} (UtxoSet.Unspent output) utxoSet

    let state = { state with memoryState = { state.memoryState with utxoSet = utxoSet } }

    TransactionHandler.executeContract session sampleInputTx 1_000_000UL contractId "" None None state false
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output is spent``() =
    let output = {lock=PK Hash.zero;spend={asset=Asset.Zen;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} (UtxoSet.Spent output) utxoSet

    let state = { state with memoryState = { state.memoryState with utxoSet = utxoSet } }

    TransactionHandler.executeContract session sampleInputTx 1_000_000UL contractId "" None None state false
    |> shouldBeErrorMessage "not enough Zens"
