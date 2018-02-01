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

let chain = ChainParameters.Test

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

let account = Account.createRoot ()

let shouldBeOk result =
    result
    |> Result.mapError failwith
    |> ignore

let shouldBeErrorMessage message =
    function
    | Ok _ -> failwithf "Expected '%A' error message, got ok" message
    | Error err -> err |> should equal message

let activateContract code account session state =
    Account.createActivateContractTransaction account code
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
    open Zen.Assets
    open FStar.Mul

    module ET = Zen.ErrorT
    module Tx = Zen.TxSkeleton

    val cf: txSkeleton -> string -> #l:nat -> wallet l -> cost nat 9
    let cf _ _ #l _ = ret (l * 128 + 192 + 13 + 64)

    val main: txSkeleton -> hash -> string -> #l:nat -> wallet l -> cost (result txSkeleton) (l * 128 + 192 + 13 + 64)
    let main txSkeleton contractHash command #l wallet =
    let result =
    Tx.lockToPubKey zenAsset 10UL (hashFromBase64 "DYggLLPq6eXj1YxjiPQ5dSvb/YVqAVNf8Mjnpc9P9BI=") txSkeleton
    >>= Tx.fromWallet zenAsset 10UL contractHash wallet in

    ET.of_optionT "not enough Zens" result
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
    let output = {lock=Contract cHash;spend={asset=Hash.zero;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} (UtxoSet.Unspent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx cHash "" { state.memoryState with utxoSet = utxoSet } 
    |> shouldBeOk

[<Test>]
let ``Contract should not be able to lock more token than available``() =
    let output = {lock=Contract cHash;spend={asset=Hash.zero;amount=9UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=1ul} (UtxoSet.Unspent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx cHash "" { state.memoryState with utxoSet = utxoSet } 
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output is missing``() =
    
    TransactionHandler.executeContract session sampleInputTx cHash "" state.memoryState
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output locked to PK address``() =
    let output = {lock=PK cHash;spend={asset=Hash.zero;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} (UtxoSet.Unspent output) utxoSet

    TransactionHandler.executeContract session sampleInputTx cHash "" { state.memoryState with utxoSet = utxoSet } 
    |> shouldBeErrorMessage "not enough Zens"

[<Test>]
let ``Contract should not have enough tokens when output is spent``() =
    let output = {lock=PK cHash;spend={asset=Hash.zero;amount=10UL}}
    let utxoSet = Map.add {txHash=Hash.zero;index=10ul} UtxoSet.Spent utxoSet

    TransactionHandler.executeContract session sampleInputTx cHash "" { state.memoryState with utxoSet = utxoSet } 
    |> shouldBeErrorMessage "not enough Zens"

