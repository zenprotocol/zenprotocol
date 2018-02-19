module Blockchain.Tests.ValidateTransactionTests

// This tests only for ValidateTransaction command
// We don't need to cover the entire outcome of transaction validation
// we have transaction validation tests for that.
// We are checking that all events are raised, tx enter mempool, utxoset or orphan tx list

open NUnit.Framework
open FsUnit
open DataAccess
open Blockchain
open Consensus
open Consensus.Types
open Wallet
open Messaging.Services.Blockchain
open Messaging.Events
open Infrastructure
open Consensus.Tests.ContractTests
open Blockchain.State
open Consensus.Tests.SampleContract
open TestsInfrastructure.Constraints

type TransactionResult = Messaging.Services.TransactionResult

let chain = ChainParameters.Local
// Helper functions for the tests
let getStringBytes (str : string) = System.Text.Encoding.UTF8.GetBytes str
let getStringHash = getStringBytes >> Hash.compute
let createTransaction address amount account =
    match Account.createTransaction chain account address { asset = Constants.Zen; amount = amount } with
    | Result.Ok tx -> tx
    | Result.Error error -> failwith error
let getTxOutpoints txHash tx = [ for i in 0 .. List.length tx.outputs - 1 -> {txHash=txHash;index= uint32 i} ]
let areOutpointsInSet session outpoints set =
    Option.isSome
        <| UtxoSet.getUtxos (UtxoSetRepository.get session) outpoints set

// Some default transaction to work with during the tests
let tx =
    let account = Account.createTestAccount ()
    createTransaction account.publicKeyHash 1UL account
let txHash = Transaction.hash tx
let txOutpoints = getTxOutpoints txHash tx

// Default initial state of mempool and utxoset
let utxoSet = UtxoSet.asDatabase |> UtxoSet.handleTransaction (fun _ -> UtxoSet.NoOutput) Transaction.rootTxHash Transaction.rootTx
let mempool = MemPool.empty |> MemPool.add Transaction.rootTxHash Transaction.rootTx
let orphanPool = OrphanPool.create()
let acs = ActiveContractSet.empty

let state = {
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

[<Test>]
let ``valid transaction raise events and update state``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext
    let result = Handler.handleCommand chain (ValidateTransaction tx) session 1UL state

    let events, state' = Writer.unwrap result

    // Checking that the event raised
    events |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (txHash, tx)))

    // Checking only 1 event raised
    events |> should haveLength 1

    // Checking the tx is in the mempool
    MemPool.containsTransaction txHash state'.memoryState.mempool |> should equal true

    // Checking the tx is in the utxoset
    UtxoSet.getUtxos (UtxoSetRepository.get session) txOutpoints state'.memoryState.utxoSet
    |> should equal (Some tx.outputs)

[<Test>]
let ``Invalid tx doesn't raise events or update state``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let invalidTx = {inputs=[];outputs=[];witnesses=[];contract=None}

    let result = Handler.handleCommand chain (ValidateTransaction invalidTx) session 1UL state

    let events, state' = Writer.unwrap result

    // Checking no events raised
    events |> should haveLength 0

    // Checking the tx is not in the mempool
    MemPool.containsTransaction txHash state'.memoryState.mempool |> should equal false

    // Checking the tx is not in the utxoset
    UtxoSet.getUtxos (UtxoSetRepository.get session) txOutpoints state'.memoryState.utxoSet |> should equal None

    // Orphan should be empty
    Map.isEmpty state'.memoryState.orphanPool |> should equal true

[<Test>]
let ``tx already in mempool nothing happen`` () =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let result = Handler.handleCommand chain (ValidateTransaction Transaction.rootTx) session 1UL state

    let events, state' = Writer.unwrap result

    // Checking no events raised
    events |> should haveLength 0

    // Checking that state didn't change
    state'.memoryState.utxoSet |> should equal utxoSet
    state'.memoryState.mempool |> should equal mempool

[<Test>]
let ``orphan tx added to orphan list``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let utxoSet = UtxoSet.asDatabase
    let state = {state with memoryState={state.memoryState with utxoSet=utxoSet}}

    let result = Handler.handleCommand chain (ValidateTransaction tx) session 1UL state

    let events, state' = Writer.unwrap result

    // Checking no events raised
    events |> should haveLength 0

    // Checking the tx is not in the mempool
    MemPool.containsTransaction txHash state'.memoryState.mempool |> should equal false

    // Checking the tx is not in the utxoset
    UtxoSet.getUtxos (UtxoSetRepository.get session) txOutpoints state'.memoryState.utxoSet |> should equal None

    OrphanPool.containsTransaction txHash state'.memoryState.orphanPool |> should equal true

[<Test>]
let ``origin tx hit mempool, orphan tx should be added to mempool``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let rootAccount = Account.createTestAccount ()
    let account1 = Account.create ()
    let account2 = Account.create ()

    let tx1 = createTransaction account1.publicKeyHash 1UL rootAccount
    let tx1Hash = Transaction.hash tx1

    let tx2 =
        Account.addTransaction tx1Hash tx1 account1
        |> createTransaction account2.publicKeyHash 1UL
    let tx2Hash = Transaction.hash tx2

    // Sending orphan transaction first, which should be added to orphan list
    let result = Handler.handleCommand chain (ValidateTransaction tx2) session 1UL state
    let events, state' = Writer.unwrap result

    // Checking that the transaction is only in the mempool and no event were raised
    events |> should haveLength 0
    MemPool.containsTransaction tx2Hash state'.memoryState.mempool |> should equal false
    UtxoSet.getUtxos (UtxoSetRepository.get session) (getTxOutpoints tx2Hash tx2) state'.memoryState.utxoSet |> should equal None
    OrphanPool.containsTransaction tx2Hash state'.memoryState.orphanPool |> should equal true

    // Sending origin, which should cause both transaction to be added to the mempool
    let result' = Handler.handleCommand chain (ValidateTransaction tx1) session 1UL state'
    let events', state'' = Writer.unwrap result'

    // Checking that both transaction added to mempool and published
    events' |> should haveLength 2
    events' |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (tx1Hash,tx1)))
    events' |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (tx2Hash,tx2)))
    MemPool.containsTransaction tx1Hash state''.memoryState.mempool |> should equal true
    MemPool.containsTransaction tx2Hash state''.memoryState.mempool |> should equal true
    UtxoSet.getUtxos (UtxoSetRepository.get session) (getTxOutpoints tx1Hash tx1) state''.memoryState.utxoSet |> should equal None // Was already spent by tx2
    UtxoSet.getUtxos (UtxoSetRepository.get session) (getTxOutpoints tx2Hash tx2) state''.memoryState.utxoSet |> should equal (Some tx2.outputs)
    OrphanPool.containsTransaction tx2Hash state''.memoryState.orphanPool |> should equal false
    OrphanPool.containsTransaction tx1Hash state''.memoryState.orphanPool |> should equal false

[<Test>]
let ``orphan transaction is eventually invalid``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let rootAccount = Account.createTestAccount ()
    let account1 = Account.create ()
    let account2 = Account.create ()

    let tx1 = createTransaction account1.publicKeyHash 2UL rootAccount
    let tx1Hash = Transaction.hash tx1

    let tx2 =
        let tx =
            Account.addTransaction tx1Hash tx1 account1
            |> createTransaction account2.publicKeyHash 2UL
        // let's change one of the outputs value and reassign to make invalid tx
        let output = tx.outputs.[0]
        let output' = {output with spend = {amount = output.spend.amount - 1UL; asset = output.spend.asset}}
        let outputs = output' :: List.tail tx.outputs
        let tx' = { tx with outputs = outputs}
        Transaction.sign [account1.keyPair] tx'
    let tx2Hash = Transaction.hash tx2

    // Sending orphan transaction first, which should be added to orphan list
    let result = Handler.handleCommand chain (ValidateTransaction tx2) session 1UL state
    let events, state' = Writer.unwrap result

    // Checking that the transaction is only in the mempool and no event were raised
    events |> should haveLength 0
    MemPool.containsTransaction tx2Hash state'.memoryState.mempool |> should equal false
    UtxoSet.getUtxos (UtxoSetRepository.get session) (getTxOutpoints tx2Hash tx2) state'.memoryState.utxoSet |> should equal None
    OrphanPool.containsTransaction tx2Hash state'.memoryState.orphanPool |> should equal true

     // Sending origin, which should cause orphan transaction to be rejected and removed from orphan list
    let result' = Handler.handleCommand chain (ValidateTransaction tx1) session 1UL state'
    let events', state'' = Writer.unwrap result'

    // Checking that the origin tx is published and orphan removed as invalid
    events' |> should haveLength 1
    events' |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (tx1Hash,tx1)))
    MemPool.containsTransaction tx1Hash state''.memoryState.mempool |> should equal true
    MemPool.containsTransaction tx2Hash state''.memoryState.mempool |> should equal false
    UtxoSet.getUtxos (UtxoSetRepository.get session) (getTxOutpoints tx1Hash tx1) state''.memoryState.utxoSet |> should equal (Some tx1.outputs)
    UtxoSet.getUtxos (UtxoSetRepository.get session) (getTxOutpoints tx2Hash tx2) state''.memoryState.utxoSet |> should equal None
    OrphanPool.containsTransaction tx2Hash state''.memoryState.orphanPool |> should equal false
    OrphanPool.containsTransaction tx1Hash state''.memoryState.orphanPool |> should equal false

[<Test>]
let ``two orphan transaction spending same input``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let rootAccount = Account.createTestAccount ()
    let account1 = Account.create ()
    let account2 = Account.create ()
    let account3 = Account.create ()

    let tx1 = createTransaction account1.publicKeyHash 1UL rootAccount
    let tx1Hash = Transaction.hash tx1

    let tx2 =
        Account.addTransaction tx1Hash tx1 account1
        |> createTransaction account2.publicKeyHash 1UL
    let tx2Hash = Transaction.hash tx2

    let tx3 =
        Account.addTransaction tx1Hash tx1 account1
        |> createTransaction account3.publicKeyHash 1UL
    let tx3Hash = Transaction.hash tx3

    let (>>=) = Writer.bind

    // Sending both tx2 and tx3, both should be added to orphan pool
    let result =
        Handler.handleCommand chain (ValidateTransaction tx2) session 1UL state
        >>=
        Handler.handleCommand chain (ValidateTransaction tx3) session 1UL

    let events, state' = Writer.unwrap result

    // Checking that the transaction is only in the mempool and no event were raised
    events |> should haveLength 0
    MemPool.containsTransaction tx2Hash state'.memoryState.mempool |> should equal false
    MemPool.containsTransaction tx3Hash state'.memoryState.mempool |> should equal false
    UtxoSet.getUtxos (UtxoSetRepository.get session) (getTxOutpoints tx2Hash tx2) state'.memoryState.utxoSet |> should equal None
    UtxoSet.getUtxos (UtxoSetRepository.get session) (getTxOutpoints tx3Hash tx3) utxoSet |> should equal None
    OrphanPool.containsTransaction tx2Hash state'.memoryState.orphanPool |> should equal true
    OrphanPool.containsTransaction tx3Hash state'.memoryState.orphanPool |> should equal true

    // Sending origin, which should pick one of the transactions (we cannot know which one, for now at least)
    let result' = Handler.handleCommand chain (ValidateTransaction tx1) session 1UL state'
    let events', state'' = Writer.unwrap result'

    // Checking that both transaction added to mempool and published
    events' |> should haveLength 2
    events' |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (tx1Hash,tx1)))
    MemPool.containsTransaction tx1Hash state''.memoryState.mempool |> should equal true
    MemPool.containsTransaction tx2Hash state''.memoryState.mempool <> MemPool.containsTransaction tx3Hash state''.memoryState.mempool
    |> should equal true

    UtxoSet.getUtxos (UtxoSetRepository.get session) (getTxOutpoints tx1Hash tx1) state''.memoryState.utxoSet |> should equal None // Was already spent by tx2
    areOutpointsInSet session (getTxOutpoints tx2Hash tx2) state''.memoryState.utxoSet <> areOutpointsInSet session (getTxOutpoints tx3Hash tx3) state''.memoryState.utxoSet
    |> should equal true

    OrphanPool.containsTransaction tx3Hash state''.memoryState.orphanPool |> should equal false
    OrphanPool.containsTransaction tx2Hash state''.memoryState.orphanPool |> should equal false

[<Test>]
let ``Valid contract should be added to ActiveContractSet``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let rootAccount = Account.createTestAccount ()
    let cHash = getStringHash sampleContractCode

    let tx =
        match Account.createActivateContractTransaction chain rootAccount sampleContractCode 1ul with
            | Result.Ok tx ->
                tx
            | _ ->
                failwith "couldn't get tx"

    let txHash = Transaction.hash tx

    let result =
        Handler.handleCommand chain (ValidateTransaction tx) session 1UL state

    let events, state' = Writer.unwrap result

    // Checking that the contract is in the ACS
    ActiveContractSet.containsContract cHash state'.memoryState.activeContractSet |> should equal true

    // Checking that TransactionAddedToMemPool was published
    events |> should haveLength 1
    events |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (txHash, tx)))

    // Checking that the transaction was added to mempool
    MemPool.containsTransaction txHash state'.memoryState.mempool |> should equal true

    // Checking that the transaction was not added to orphans
    OrphanPool.containsTransaction txHash state'.memoryState.orphanPool |> should equal false

[<Test>]
let ``Invalid contract should not be added to ActiveContractSet or mempool``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let rootAccount = Account.createTestAccount ()
    let contractCode = "x"
    let cHash = getStringHash contractCode

    let tx =
        let input, output = Account.getUnspentOutputs rootAccount |> Map.toSeq |> Seq.head
        let output' = {output with lock=PK rootAccount.publicKeyHash}
        { inputs=[ input ]; outputs=[ output' ]; witnesses=[]; contract = Some (contractCode, "") }
        |> (Transaction.sign [ rootAccount.keyPair ])

    let txHash = Transaction.hash tx

    let result =
        Handler.handleCommand chain (ValidateTransaction tx) session 1UL state

    let events, state' = Writer.unwrap result

    // Checking that the contract is not in the ACS
    ActiveContractSet.containsContract cHash state'.memoryState.activeContractSet |> should equal false

    events |> should haveLength 0

    // Checking that the transaction was not added to mempool
    MemPool.containsTransaction txHash state'.memoryState.mempool |> should equal false

    // Checking that the transaction was not added to orphans
    OrphanPool.containsTransaction txHash state'.memoryState.orphanPool |> should equal false

[<Test>]
let ``contract activation arrived, running orphan transaction``() =
    let getResult = function
        | Ok r -> r
        | Error error -> failwithf "%A" error

    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let account = Account.createTestAccount ()

    let activationTransaction =
        Account.createActivateContractTransaction chain account sampleContractCode 1ul
        |> getResult
    let activationTxHash = Transaction.hash activationTransaction

    let state = { state with memoryState = { state.memoryState with utxoSet = getSampleUtxoset utxoSet } }

    let _, stateWithContract =
        Handler.handleCommand chain (ValidateTransaction activationTransaction) session 1UL state
        |> Writer.unwrap

    let tx =
        TransactionHandler.executeContract session sampleInputTx sampleContractHash "" Contract.EmptyData (PK Hash.zero) stateWithContract.memoryState
        |> getResult
    let txHash = Transaction.hash tx

    let events, state =
        Handler.handleCommand chain (ValidateTransaction tx) session 1UL state
        |> Writer.unwrap

    // Checking that both transaction added to mempool and published
    events |> should haveLength 0
    OrphanPool.containsTransaction txHash state.memoryState.orphanPool |> should equal true

    // This will actually fail and the transaction will not pass, however it is enough for our test
    // to see that the transaction was run and removed from orphan pool as it is not valid
    let events, state =
        Handler.handleCommand chain (ValidateTransaction activationTransaction) session 1UL state
        |> Writer.unwrap

    events |> should haveLength 1
    OrphanPool.containsTransaction txHash state.memoryState.orphanPool |> should equal false
    MemPool.containsTransaction activationTxHash state.memoryState.mempool |> should equal true
    MemPool.containsTransaction txHash state.memoryState.mempool |> should equal false
    events |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (activationTxHash,activationTransaction)))

[<Test>]
let ``Transaction already in db but not part of the main chain``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext

    // we need to fake some information
    let header =
        {
            version = 0ul
            parent = Hash.zero
            blockNumber = 2ul
            commitments = Hash.zero
            timestamp = 0UL
            difficulty = 0ul
            nonce = 0UL,0UL
        }
    let extendedHeader : ExtendedBlockHeader.T =
        {
            hash = BlockHeader.hash header
            header = header
            status = ExtendedBlockHeader.Connected
            chainWork = None
            txMerkleRoot = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            witnessMerkleRoot = Hash.zero
            commitments = []
        }

    Collection.put session.context.blocks session.session extendedHeader.hash extendedHeader
    MultiCollection.put session.context.transactionBlocks session.session txHash extendedHeader.hash

    let result = Handler.handleCommand chain (ValidateTransaction tx) session 1UL state

    let events, state' = Writer.unwrap result

    // Checking that the event raised
    events |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (txHash, tx)))

    // Checking only 1 event raised
    events |> should haveLength 1

    // Checking the tx is in the mempool
    MemPool.containsTransaction txHash state'.memoryState.mempool |> should equal true

    // Checking the tx is in the utxoset
    UtxoSet.getUtxos (UtxoSetRepository.get session) txOutpoints state'.memoryState.utxoSet
    |> should equal (Some tx.outputs)

[<Test>]
let ``Transaction already in db and part of the main chain is ignored``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext

    // we need to fake some information
    let header =
        {
            version = 0ul
            parent = Hash.zero
            blockNumber = 2ul
            commitments = Hash.zero
            timestamp = 0UL
            difficulty = 0ul
            nonce = 0UL,0UL
        }
    let extendedHeader : ExtendedBlockHeader.T =
        {
            hash = BlockHeader.hash header
            header = header
            status = ExtendedBlockHeader.MainChain
            chainWork = None
            txMerkleRoot = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            witnessMerkleRoot = Hash.zero
            commitments = []
        }

    Collection.put session.context.blocks session.session extendedHeader.hash extendedHeader
    MultiCollection.put session.context.transactionBlocks session.session txHash extendedHeader.hash

    let result = Handler.handleCommand chain (ValidateTransaction tx) session 1UL state

    let events, state' = Writer.unwrap result

    // Checking only 1 event raised
    events |> should haveLength 0

    // Checking the tx is in the mempool
    MemPool.containsTransaction txHash state'.memoryState.mempool |> should equal false