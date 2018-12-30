module Blockchain.Tests.MintingContractTests

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
open Zen
open Helper
module Result = Core.Result

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
            invalidTxHashes = FSharp.Collections.Set.empty
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

let tempDir () =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]
let dataPath = tempDir()
let databaseContext = DatabaseContext.createTemporary dataPath
let session = DatabaseContext.createSession databaseContext

let mutable contractId = ContractId (Version0, Hash.zero)

let clean() =
    Platform.cleanDirectory dataPath

[<OneTimeSetUp>]
let setUp = fun () ->
    clean()
    activateContract """
    open Zen.Types
    open Zen.Base
    open Zen.Cost
    open Zen.Asset
    open Zen.Data

    module D = Zen.Dictionary
    module W = Zen.Wallet
    module RT = Zen.ResultT
    module Tx = Zen.TxSkeleton
    module C = Zen.Cost

    let buy txSkeleton contractId returnAddress =
      let! contractToken = Zen.Asset.getDefault contractId in
      let! amount = Tx.getAvailableTokens zenAsset txSkeleton in

      let! txSkeleton =
        Tx.lockToContract zenAsset amount contractId txSkeleton
        >>= Tx.mint amount contractToken
        >>= Tx.lockToAddress contractToken amount returnAddress in
      RT.ok @ { tx = txSkeleton; message = None; state = NoChange }

    let redeem txSkeleton contractId returnAddress wallet =
      let! contractToken = Zen.Asset.getDefault contractId in
      let! amount = Tx.getAvailableTokens contractToken txSkeleton in

      let! txSkeleton =
        Tx.destroy amount contractToken txSkeleton
        >>= Tx.lockToAddress zenAsset amount returnAddress
        >>= Tx.fromWallet zenAsset amount contractId wallet in

      let result =
          match txSkeleton with
          | Some tx -> Some @ { tx = tx; message = None; state = NoChange }
          | None -> None in

      RT.ofOption "contract doesn't have enough zens tokens" result

    let main txSkeleton _ contractId command sender messageBody wallet state =
      let! returnAddress =
        messageBody >!= tryDict
                    >?= D.tryFind "returnAddress"
                    >?= tryLock
      in
      match returnAddress with
      | Some returnAddress ->
        if command = "redeem" then
          redeem txSkeleton contractId returnAddress wallet
        else if command = "" || command = "buy" then
          buy txSkeleton contractId returnAddress
          |> autoInc
        else
          RT.autoFailw "unsupported command"
      | None ->
        RT.autoFailw "returnAddress is required"

    let cf _ _ _ _ _ wallet _ =
        4 + 64 + 2 + (64 + (64 + (64 + 64 + (Zen.Wallet.size wallet * 128 + 192) + 0)) + 33) + 31
        |> cast nat
        |> C.ret

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

open Crypto
open TxSkeleton

let sampleKeyPair = KeyPair.create()
let samplePrivateKey, samplePublicKey = sampleKeyPair
let samplePKHash = PublicKey.hash samplePublicKey

let Zen = Asset.Zen

[<Test>]
let ``Contract should detect unsupported command``() =
    let inputTx =
        {
            pInputs = [ ]
            outputs = [ ]
        }

    let returnAddress =
                PK samplePKHash
                |> ZFStar.fsToFstLock
                |> Types.Data.Lock

    let data =
        Dictionary.add "returnAddress"B returnAddress  Dictionary.empty
        |> Cost.Realized.__force
        |> Types.Data.Dict
        |> Types.Data.Collection
        |> Some

    TransactionHandler.executeContract session inputTx 1_000_000UL contractId "x" None data state false
    |> shouldBeErrorMessage "unsupported command"

[<Test>]
let ``Should buy``() =
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
        Map.add input (UtxoSet.Unspent output) Map.empty

    let inputTx =
        {
            pInputs = [ PointedOutput (input, output) ]
            outputs = [ ]
        }

    let returnAddress =
        PK samplePKHash
        |> ZFStar.fsToFstLock
        |> Types.Data.Lock

    let data =
        Dictionary.add "returnAddress"B returnAddress  Dictionary.empty
        |> Cost.Realized.__force
        |> Types.Data.Dict
        |> Types.Data.Collection
        |> Some

    let state = { state with memoryState = { state.memoryState with utxoSet = utxoSet } }

    TransactionHandler.executeContract session inputTx 1_000_000UL contractId "buy" None data state false
    |> function
    | Ok tx ->
        tx.inputs |> should haveLength 2
        tx.inputs |> should contain (Outpoint input)
        tx.outputs |> should haveLength 2
        tx.outputs |> should contain { lock = Contract contractId; spend = spend }
        tx.outputs |> should contain { lock = PK samplePKHash; spend = { spend with asset = Asset.defaultOf contractId } }
        tx.witnesses |> should haveLength 1
        let wit, cost =
            match tx.witnesses.[0] with
            | ContractWitness {cost=cost} as wit ->
                wit, cost
            | _ as wit -> wit, 0UL

        let cw = ContractWitness {
             contractId = contractId
             command = "buy"
             messageBody = data
             stateCommitment = NotCommitted
             beginInputs = 1u
             beginOutputs = 0u
             inputsLength = 1u
             outputsLength = 2u
             signature = None
             cost = cost
        }
        wit |> should equal cw
    | Error error -> failwith error

[<Test>]
let ``Should redeem``() =
    let inputZen = {
        txHash = Hash.zero
        index = 1u
    }

    let inputContractAsset = {
        txHash = Hash.zero
        index = 2u
    }

    let spendZen = { asset = Zen; amount = 5UL }
    let spendContractAsset = { asset = Asset.defaultOf contractId; amount = 5UL }

    let outputZen = {
        lock = Contract contractId
        spend = spendZen
    }

    let outputContractAsset = {
        lock = PK samplePKHash
        spend = spendContractAsset
    }

    let utxoSet =
        Map.empty
        |> Map.add inputZen (UtxoSet.Unspent outputZen)
        |> Map.add inputContractAsset (UtxoSet.Unspent outputContractAsset)

    let inputTx =
        {
            pInputs = [ PointedOutput (inputContractAsset, outputContractAsset) ]
            outputs = [ ]
        }

    let returnAddress =
        PK samplePKHash
        |> ZFStar.fsToFstLock
        |> Types.Data.Lock

    let data =
        Dictionary.add "returnAddress"B returnAddress  Dictionary.empty
        |> Cost.Realized.__force
        |> Types.Data.Dict
        |> Types.Data.Collection
        |> Some

    let state = { state with memoryState = { state.memoryState with utxoSet = utxoSet } }

    TransactionHandler.executeContract session inputTx 1_000_000UL contractId "redeem" None data state false
    |> function
    | Ok tx ->
        tx.inputs |> should haveLength 2
        tx.inputs |> should contain (Outpoint inputContractAsset)
        tx.inputs |> should contain (Outpoint inputZen)
        tx.outputs |> should haveLength 2
        tx.outputs |> should contain { lock = Destroy; spend = spendContractAsset }
        tx.outputs |> should contain { lock = PK samplePKHash; spend = spendZen }
        tx.witnesses |> should haveLength 1
        let wit, cost =
            match tx.witnesses.[0] with
            | ContractWitness {cost=cost} as wit ->
                wit, cost
            | _ as wit -> wit, 0UL

        let cw = ContractWitness {
             contractId = contractId
             command = "redeem"
             messageBody = data
             stateCommitment = NotCommitted
             beginInputs = 1u
             beginOutputs = 0u
             inputsLength = 1u
             outputsLength = 2u
             signature = None
             cost = cost
        }
        wit |> should equal cw
    | Error error -> failwith error
