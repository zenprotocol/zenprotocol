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
open TestsInfrastructure.Constraints
open Consensus.Tests.SampleContract

let chain = Chain.getChainParameters Chain.Local

let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction (fun _ -> UtxoSet.NoOutput) Transaction.rootTxHash Transaction.rootTx
let mempool = MemPool.empty |> MemPool.add Transaction.rootTxHash Transaction.rootTx
let orphanPool = OrphanPool.create()
let acs = ActiveContractSet.empty

let mutable state = {
    memoryState =
        {
            utxoSet = utxoSet
            mempool = mempool
            orphanPool = orphanPool
            activeContractSet = acs
        }
    tipState =
        {
            tip = ExtendedBlockHeader.empty
            activeContractSet = acs
            ema=EMA.create chain
        }
    blockRequests= Map.empty
}

let account = Account.createTestAccount ()

let shouldBeOk result =
    result
    |> Result.mapError failwith
    |> ignore

let shouldBeErrorMessage message =
    function
    | Ok _ -> failwithf "Expected '%A' error message, got ok" message
    | Error err -> err |> should equal message

let activateContract code account session state =
    Account.createActivateContractTransaction chain account code 1ul
    |> Result.map (fun tx ->
        let events, state =
            Handler.handleCommand chain (ValidateTransaction tx) session 1UL state
            |> Writer.unwrap
        let txHash = Transaction.hash tx
        events |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (txHash, tx)))
        let cHash = code |> System.Text.Encoding.UTF8.GetBytes |> Hash.compute
        ActiveContractSet.containsContract cHash state.memoryState.activeContractSet
        |> should equal true
        (state, cHash)
    )

let dataPath = ".data"
let databaseContext = DatabaseContext.createEmpty dataPath
let session = DatabaseContext.createSession databaseContext

let mutable cHash = Hash.zero

let clean() =
    Platform.cleanDirectory dataPath

[<OneTimeSetUp>]
let setUp = fun () ->
    clean()
    activateContract """
    open Zen.Types
    open Zen.Vector
    open Zen.Util
    open Zen.Base
    open Zen.Cost
    open Zen.Asset

    module ET = Zen.ErrorT
    module Tx = Zen.TxSkeleton

    val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 11
    let cf _ _ _ _ #l _ =
        let res : nat = (64 + (l * 128 + 192) + 0 + 20) in
        ret res

    val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message)) ((64 + (l * 128 + 192) + 0 + 20) <: nat)
    let main txSkeleton contractHash command data returnAddress #l wallet =
    let! result =
        Tx.lockToPubKey zenAsset 10UL (hashFromBase64 "DYggLLPq6eXj1YxjiPQ5dSvb/YVqAVNf8Mjnpc9P9BI=") txSkeleton
        >>= Tx.fromWallet zenAsset 10UL contractHash wallet in

    let result' =
        match result with
        | Some tx -> Some (tx, None)
        | None -> None in

    ET.of_option "not enough Zens" result'
    """ account session state
    |> function
    | Ok (state', cHash') ->
        cHash <- cHash'
        state <- state'
    | Error error ->
        failwith error

[<TearDown>]
let tearDown = fun () ->
    clean()

[<Test>]
let ``Wallet using contract should execute``() =
    let output = {lock=Contract cHash;spend={asset=Constants.Zen;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} (UtxoSet.Unspent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx cHash "" Contract.EmptyData (PK Hash.zero) { state.memoryState with utxoSet = utxoSet }
    |> shouldBeOk

[<Test>]
let ``Contract should not be able to lock more token than available``() =
    let output = {lock=Contract cHash;spend={asset=Constants.Zen;amount=9UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=1ul} (UtxoSet.Unspent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx cHash "" Contract.EmptyData (PK Hash.zero) { state.memoryState with utxoSet = utxoSet }
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output is missing``() =

    TransactionHandler.executeContract session sampleInputTx cHash "" Contract.EmptyData (PK Hash.zero) state.memoryState
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output locked to PK address``() =
    let output = {lock=PK cHash;spend={asset=Constants.Zen;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} (UtxoSet.Unspent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx cHash "" Contract.EmptyData (PK Hash.zero) { state.memoryState with utxoSet = utxoSet }
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output is spent``() =
    let output = {lock=PK cHash;spend={asset=Constants.Zen;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} (UtxoSet.Spent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx cHash "" Contract.EmptyData (PK Hash.zero) { state.memoryState with utxoSet = utxoSet }
    |> shouldBeErrorMessage "not enough Zens"
