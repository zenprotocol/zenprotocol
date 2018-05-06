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
open Consensus.Tests

open TestsInfrastructure.Constraints
open Crypto
open TxSkeleton
open Zen
open Helper
module Result = Core.Result

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
    headers = 0ul
}

let account = createTestAccount()

let shouldBeErrorMessage message =
    function
    | Ok _ -> failwithf "Expected '%A' error message, got ok" message
    | Error err -> err |> should equal message

let activateContract code account session state =
    let (account:Account.T), secretKey = account
    let account = { account with mempool = MemPool.toList state.memoryState.mempool }

    let accountData = account, secretKey
    Account.createActivateContractTransaction chain code 1ul accountData
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

let tempDir () =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]
let dataPath = tempDir()
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
open Zen.Base
open Zen.Cost
open Zen.Asset

module RT = Zen.ResultT
module Tx = Zen.TxSkeleton

let main txSkeleton contractHash command sender data wallet =
    let isFromContract =
        match sender with
        | Contract contractHash' -> contractHash' <> contractHash
        | _ -> false in

    if isFromContract && command = "contract2_test" then
    begin
        let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in

        let! txSkeleton =
            Tx.lockToContract zenAsset tokens contractHash txSkeleton in
        RT.ok (txSkeleton, None)
    end
    else
        RT.autoFailw "unsupported command"

val cf: txSkeleton -> string -> sender -> option data -> wallet -> cost nat 7
let cf _ _ _ _ _ = ret (64 + 64 + 0 + 25)
"""
let contract2Id = Contract.makeContractId Version0 contract2Code

let contract1Code =
    contract2Id
    |> ContractId.toBytes
    |> System.Convert.ToBase64String
    |> sprintf """
// contract 1: this contract receives Zen tokens; mints and locks it's own tokens to the return-address, and passes a message to contract 2

open Zen.Types
open Zen.Util
open Zen.Base
open Zen.Cost
open Zen.Asset
open Zen.Data

module RT = Zen.ResultT
module Tx = Zen.TxSkeleton

let main txSkeleton contractHash command sender data wallet =
    let! returnAddress = data >!> tryDict >?> tryFindLock "returnAddress" in

    match returnAddress with
    | Some returnAddress ->
        let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in
        let! asset = Zen.Asset.getDefault contractHash in
        let! txSkeleton =
            Tx.mint tokens asset txSkeleton
            >>= Tx.lockToAddress asset tokens returnAddress in

        let message = {
            contractId = contractIdFromBase64 "%s";
            command = "contract2_test";
            data
        } in

        RT.ok (txSkeleton, Some message)
    | None ->
        RT.autoFailw "returnAddress is required"

val cf: txSkeleton -> string -> sender -> option data -> wallet -> cost nat 15
let cf _ _ _ _ _ = ret (3 + 66 + (64 + (64 + (64 + 64 + 0))) + 34)
"""

[<Test>]
[<ParallelizableAttribute>]
let ``Should execute contract chain and get a valid transaction``() =
    let sampleKeyPair = KeyPair.create()
    let _, samplePublicKey = sampleKeyPair
    let samplePKHash = PublicKey.hash samplePublicKey

    let Zen = Asset.Zen

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
        let! (state, contractId1) = activateContract contract1Code account session state
        let! (state, _) = activateContract contract2Code account session state

        let returnAddress =
            PK samplePKHash
            |> ZFStar.fsToFstLock
            |> Types.Data.Lock

        let data =
            Dictionary.add "returnAddress"B returnAddress  Dictionary.empty
            |> Cost.Realized.__force
            |> Types.Data.DataDict
            |> Types.Data.Dict
            |> Some

        let! tx = TransactionHandler.executeContract session inputTx contractId1 "" None data state.memoryState

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
