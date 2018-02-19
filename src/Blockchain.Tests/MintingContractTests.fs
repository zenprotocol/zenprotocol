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
open TestsInfrastructure.Constraints

let chain = ChainParameters.Local

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
    open Zen.Base
    open Zen.Cost
    open Zen.Asset

    module ET = Zen.ErrorT
    module Tx = Zen.TxSkeleton

    val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 19
    let cf _ _ _ _ #l _ = ret (64 + (64 + (64 + 64 + (l * 128 + 192) + 0)) + 28 + 22)

    let buy txSkeleton contractHash returnAddress =
      let! contractToken = Zen.Asset.getDefault contractHash in
      let! amount = Tx.getAvailableTokens zenAsset txSkeleton in

      let! txSkeleton =
        Tx.lockToContract zenAsset amount contractHash txSkeleton
        >>= Tx.mint amount contractToken
        >>= Tx.lockToAddress contractToken amount returnAddress in
      ET.ret (txSkeleton, None)

    let redeem #l txSkeleton contractHash returnAddress (wallet:wallet l) =
      let! contractToken = Zen.Asset.getDefault contractHash in
      let! amount = Tx.getAvailableTokens contractToken txSkeleton in

      let! txSkeleton =
        Tx.destroy amount contractToken txSkeleton
        >>= Tx.lockToAddress zenAsset amount returnAddress
        >>= Tx.fromWallet zenAsset amount contractHash wallet in

      let result =
          match txSkeleton with
          | Some tx -> Some (tx, None)
          | None -> None in

      ET.of_option "contract doesn't have enough zens to pay you" result

    val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message)) (64 + (64 + (64 + 64 + (l * 128 + 192) + 0)) + 28 + 22)
    let main txSkeleton contractHash command data returnAddress #l wallet =
      match returnAddress with
      | Some returnAddress ->
        if command = "redeem" then
          redeem txSkeleton contractHash returnAddress wallet
        else if command = "" || command = "buy" then
          buy txSkeleton contractHash returnAddress
          |> autoInc
        else
          ET.autoFailw "unsupported command"
      | None ->
        ET.autoFailw "returnAddress is required"
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

open Crypto
open TxSkeleton

let sampleKeyPair = KeyPair.create()
let samplePrivateKey, samplePublicKey = sampleKeyPair
let samplePKHash = PublicKey.hash samplePublicKey

let Zen = Constants.Zen

[<Test>]
let ``Contract should detect unsupported command``() =
    let inputTx =
        {
            pInputs = [ ]
            outputs = [ ]
        }
    TransactionHandler.executeContract session inputTx cHash "x" Contract.EmptyData (PK samplePKHash) state.memoryState
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

    TransactionHandler.executeContract session inputTx cHash "buy" Contract.EmptyData (PK samplePKHash) { state.memoryState with utxoSet = utxoSet }
    |> function
    | Ok tx ->
        tx.inputs |> should haveLength 1
        tx.inputs |> should contain input
        tx.outputs |> should haveLength 2
        tx.outputs |> should contain { lock = Contract cHash; spend = spend }
        tx.outputs |> should contain { lock = PK samplePKHash; spend = { spend with asset = cHash, Hash.zero } }
        tx.witnesses |> should haveLength 1
        let cw = ContractWitness {
             cHash = cHash
             command = "buy"
             data = Contract.EmptyData
             returnAddressIndex = Some 1ul
             beginInputs = 1u
             beginOutputs = 0u
             inputsLength = 1u
             outputsLength = 2u
        }
        tx.witnesses |> should contain cw
    | Error error -> failwithf "--> %A" error

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
    let spendContractAsset = { asset = cHash, Hash.zero; amount = 5UL }

    let outputZen = {
        lock = Contract cHash
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

    TransactionHandler.executeContract session inputTx cHash "redeem" Contract.EmptyData (PK samplePKHash) { state.memoryState with utxoSet = utxoSet }
    |> function
    | Ok tx ->
        tx.inputs |> should haveLength 2
        tx.inputs |> should contain inputContractAsset
        tx.inputs |> should contain inputZen
        tx.outputs |> should haveLength 2
        tx.outputs |> should contain { lock = Destroy; spend = spendContractAsset }
        tx.outputs |> should contain { lock = PK samplePKHash; spend = spendZen }
        tx.witnesses |> should haveLength 1
        let cw = ContractWitness {
             cHash = cHash
             command = "redeem"
             data = Contract.EmptyData
             returnAddressIndex = Some 1ul
             beginInputs = 1u
             beginOutputs = 0u
             inputsLength = 1u
             outputsLength = 2u
        }
        tx.witnesses |> should contain cw
    | Error error -> failwith error
