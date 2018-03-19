module Blockchain.Tests.ContractMessageTests

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
open Crypto
open TxSkeleton

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
    headers = 0ul
}

let account = Account.createTestAccount ()

let shouldBeErrorMessage message =
    function
    | Ok _ -> failwithf "Expected '%A' error message, got ok" message
    | Error err -> err |> should equal message

let activateContract code account session state =
    Account.createActivateContractTransaction chain { account with mempool = Map.toList state.memoryState.mempool } code 1ul
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

let clean() =
    Platform.cleanDirectory dataPath

[<OneTimeSetUp>]
let setUp = fun () ->
    clean()

[<TearDown>]
let tearDown = fun () ->
    clean()

let contract2Code = """
// contract 2: this contract receives the remaining tx Zen tokens and locks them to itself

open Zen.Types
open Zen.Vector
open Zen.Base
open Zen.Cost
open Zen.Asset

module ET = Zen.ErrorT
module Tx = Zen.TxSkeleton

val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l 
    -> result (txSkeleton ** option message) `cost` (64 + 64 + 0 + 18)
let main txSkeleton contractHash command data returnAddress #l wallet =
    if command = "contract2_test" then
    begin
        let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in

        let! txSkeleton =
            Tx.lockToContract zenAsset tokens contractHash txSkeleton in
        ET.ret (txSkeleton, None)
    end
    else
        ET.autoFailw "unsupported command"

val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 7
let cf _ _ _ _ #l _ = ret (64 + 64 + 0 + 18)
"""
let contract2Hash = Contract.computeHash contract2Code

let contract1Code =
    contract2Hash
    |> Hash.bytes
    |> System.Convert.ToBase64String
    |> sprintf """
// contract 1: this contract receives Zen tokens; mints and locks it's own tokens to the return-address, and passes a message to contract 2

open Zen.Types
open Zen.Vector
open Zen.Util
open Zen.Base
open Zen.Cost
open Zen.Asset

module ET = Zen.ErrorT
module Tx = Zen.TxSkeleton

val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l 
    -> result (txSkeleton ** option message) `cost` (64 + (64 + (64 + 64 + 0)) + 28)
let main txSkeleton contractHash command data returnAddress #l wallet =
    match returnAddress with
    | Some returnAddress ->
        let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in
        let! asset = Zen.Asset.getDefault contractHash in
        let! txSkeleton =
            Tx.mint tokens asset txSkeleton
            >>= Tx.lockToAddress asset tokens returnAddress in

        let message = {
            cHash = hashFromBase64 "%s";
            command = "contract2_test";
            data
        } in

        ET.ret (txSkeleton, Some message)
    | None ->
        ET.autoFailw "returnAddress is required"

val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 11
let cf _ _ _ _ #l _ = ret (64 + (64 + (64 + 64 + 0)) + 28)
"""

[<Test>]
let ``Should execute contract chain and get a valid transaction``() =
    let sampleKeyPair = KeyPair.create()
    let _, samplePublicKey = sampleKeyPair
    let samplePKHash = PublicKey.hash samplePublicKey

    let Zen = Constants.Zen

    let input = {
        txHash = Hash.zero
        index = 1u
    }

    let spend = { asset = Zen; amount = 5UL }

    let output = {
        lock = PK (PublicKey.hash samplePublicKey)
        spend = spend
    }

    let utxoSet =
        Map.add input (UtxoSet.Unspent output) state.memoryState.utxoSet

    let memoryState = { state.memoryState with utxoSet = utxoSet }
    let state = { state with memoryState = memoryState }

    let inputTx =
        {
            pInputs = [ PointedOutput (input, output) ]
            outputs = [ ]
        }

    result {
        let! (state, cHash1) = activateContract contract1Code account session state
        let! (state, _) = activateContract contract2Code account session state
        let! tx = TransactionHandler.executeContract session inputTx cHash1 "" Contract.EmptyData (PK samplePKHash) state.memoryState

        let tx = Transaction.sign [ sampleKeyPair ] tx
        let txHash = Transaction.hash tx

        let events, memoryState =
            TransactionHandler.validateTransaction chain session dataPath 1ul tx state.memoryState
            |> Writer.unwrap

        //expect the transaction to be valid
        events |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (txHash,tx)))
        MemPool.containsTransaction txHash memoryState.mempool |> should equal true

        // meleate the last command
        let witness =
            match List.last tx.witnesses with
            | ContractWitness cw -> ContractWitness { cw with command = "x" }
            | _ -> failwith "unexpedted witness"

        let witnesses = Infrastructure.List.add witness tx.witnesses.[0 .. List.length tx.witnesses - 2]
        let tx = { tx with witnesses = witnesses }
        let txHash = Transaction.hash tx

        let events, memoryState =
            TransactionHandler.validateTransaction chain session dataPath  1ul tx state.memoryState
            |> Writer.unwrap

        //exptect the transaction to be invalid
        events |> should not' (contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (txHash,tx))))
        MemPool.containsTransaction txHash memoryState.mempool |> should equal false
    }
    |> Result.mapError failwith
    |> ignore
