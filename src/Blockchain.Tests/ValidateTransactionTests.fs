module Blockchain.Tests.ValidateTransactionTests

// This tests only for ValidateTransaction command
// We don't need to cover the entire outcome of transaction validation
// we have transaction validation tests for that.
// We are checking that all events are raised, tx enter mempool, utxoset or orphan tx list

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
open Consensus.Tests.SampleContract

open TestsInfrastructure.Nunit

type TransactionResult = Messaging.Services.TransactionResult

let chain = ChainParameters.Test
// Helper functions for the tests
let getStringBytes (str : string) = System.Text.Encoding.UTF8.GetBytes str
let getStringHash = getStringBytes >> Hash.compute
let createTransaction address amount account =
    match Account.createTransaction chain account address { asset = Hash.zero; amount = amount } with
    | Result.Ok tx -> tx
    | Result.Error error -> failwith error
let getTxOutpints txHash tx = List.mapi (fun i _ -> {txHash=txHash;index= uint32 i}) tx.outputs
let areOutpointsInSet outpoints set =
    match UtxoSet.getUtxos outpoints set with
    | Some _ -> true
    | None -> false

// Some default transaction to work with during the tests
let tx =
    let account = Account.createRoot ()
    createTransaction account.publicKeyHash 1UL account
let txHash = Transaction.hash tx
let txOutpoints = getTxOutpints txHash tx

// Default initial state of mempool and utxoset
let utxoSet = UtxoSet.create() |> UtxoSet.handleTransaction Transaction.rootTxHash Transaction.rootTx
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
            utxoSet = utxoSet
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
    UtxoSet.getUtxos txOutpoints state'.memoryState.utxoSet |> should equal (Some tx.outputs)

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
    UtxoSet.getUtxos txOutpoints state'.memoryState.utxoSet |> should equal None

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
    let utxoSet = UtxoSet.create()
    let state = {state with memoryState={state.memoryState with utxoSet=utxoSet}}

    let result = Handler.handleCommand chain (ValidateTransaction tx) session 1UL state

    let events, state' = Writer.unwrap result

    // Checking no events raised
    events |> should haveLength 0

    // Checking the tx is not in the mempool
    MemPool.containsTransaction txHash state'.memoryState.mempool |> should equal false

    // Checking the tx is not in the utxoset
    UtxoSet.getUtxos txOutpoints state'.memoryState.utxoSet |> should equal None

    OrphanPool.containsTransaction txHash state'.memoryState.orphanPool |> should equal true

[<Test>]
let ``origin tx hit mempool, orphan tx should be added to mempool``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    
    use session = DatabaseContext.createSession databaseContext
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let account2 = Account.create ()

    let tx1 = createTransaction account1.publicKeyHash 1UL rootAccount
    let tx1Hash = Transaction.hash tx1

    let tx2 =
        Account.handleTransaction tx1Hash tx1 account1
        |> createTransaction account2.publicKeyHash 1UL
    let tx2Hash = Transaction.hash tx2

    // Sending orphan transaction first, which should be added to orphan list
    let result = Handler.handleCommand chain (ValidateTransaction tx2) session 1UL state
    let events, state' = Writer.unwrap result

    // Checking that the transaction is only in the mempool and no event were raised
    events |> should haveLength 0
    MemPool.containsTransaction tx2Hash state'.memoryState.mempool |> should equal false
    UtxoSet.getUtxos (getTxOutpints tx2Hash tx2) state'.memoryState.utxoSet |> should equal None
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
    UtxoSet.getUtxos (getTxOutpints tx1Hash tx1) state''.memoryState.utxoSet |> should equal None // Was already spent by tx2
    UtxoSet.getUtxos (getTxOutpints tx2Hash tx2) state''.memoryState.utxoSet |> should equal (Some tx2.outputs)
    OrphanPool.containsTransaction tx2Hash state''.memoryState.orphanPool |> should equal false
    OrphanPool.containsTransaction tx1Hash state''.memoryState.orphanPool |> should equal false

[<Test>]
let ``orphan transaction is eventually invalid``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    
    use session = DatabaseContext.createSession databaseContext
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let account2 = Account.create ()

    let tx1 = createTransaction account1.publicKeyHash 2UL rootAccount
    let tx1Hash = Transaction.hash tx1

    let tx2 =
        let tx =
            Account.handleTransaction tx1Hash tx1 account1
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
    UtxoSet.getUtxos (getTxOutpints tx2Hash tx2) state'.memoryState.utxoSet |> should equal None
    OrphanPool.containsTransaction tx2Hash state'.memoryState.orphanPool |> should equal true

     // Sending origin, which should cause orphan transaction to be rejected and removed from orphan list
    let result' = Handler.handleCommand chain (ValidateTransaction tx1) session 1UL state'
    let events', state'' = Writer.unwrap result'

    // Checking that the origin tx is published and orphan removed as invalid
    events' |> should haveLength 1
    events' |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (tx1Hash,tx1)))
    MemPool.containsTransaction tx1Hash state''.memoryState.mempool |> should equal true
    MemPool.containsTransaction tx2Hash state''.memoryState.mempool |> should equal false
    UtxoSet.getUtxos (getTxOutpints tx1Hash tx1) state''.memoryState.utxoSet |> should equal (Some tx1.outputs)
    UtxoSet.getUtxos (getTxOutpints tx2Hash tx2) state''.memoryState.utxoSet |> should equal None
    OrphanPool.containsTransaction tx2Hash state''.memoryState.orphanPool |> should equal false
    OrphanPool.containsTransaction tx1Hash state''.memoryState.orphanPool |> should equal false

[<Test>]
let ``two orphan transaction spending same input``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    
    use session = DatabaseContext.createSession databaseContext
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let account2 = Account.create ()
    let account3 = Account.create ()

    let tx1 = createTransaction account1.publicKeyHash 1UL rootAccount
    let tx1Hash = Transaction.hash tx1

    let tx2 =
        Account.handleTransaction tx1Hash tx1 account1
        |> createTransaction account2.publicKeyHash 1UL
    let tx2Hash = Transaction.hash tx2

    let tx3 =
        Account.handleTransaction tx1Hash tx1 account1
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
    UtxoSet.getUtxos (getTxOutpints tx2Hash tx2) state'.memoryState.utxoSet |> should equal None
    UtxoSet.getUtxos (getTxOutpints tx3Hash tx3) utxoSet |> should equal None
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

    UtxoSet.getUtxos (getTxOutpints tx1Hash tx1) state''.memoryState.utxoSet |> should equal None // Was already spent by tx2
    areOutpointsInSet (getTxOutpints tx2Hash tx2) state''.memoryState.utxoSet <> areOutpointsInSet (getTxOutpints tx3Hash tx3) state''.memoryState.utxoSet
    |> should equal true

    OrphanPool.containsTransaction tx3Hash state''.memoryState.orphanPool |> should equal false
    OrphanPool.containsTransaction tx2Hash state''.memoryState.orphanPool |> should equal false

[<Test>]
let ``Valid contract should be added to ActiveContractSet``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    
    use session = DatabaseContext.createSession databaseContext
    let rootAccount = Account.createRoot ()
    let cHash = getStringHash sampleContractCode

    let tx =
        match Account.createActivateContractTransaction rootAccount sampleContractCode with
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
let ``Invalid contract should not be added to ActiveContractSet``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    
    use session = DatabaseContext.createSession databaseContext
    let rootAccount = Account.createRoot ()
    let contractCode = "x"
    let cHash = getStringHash contractCode

    let tx =
        match Account.createActivateContractTransaction rootAccount contractCode with
            | Result.Ok tx -> tx
            | _ -> failwith "couldn't get tx"

    let txHash = Transaction.hash tx

    let result =
        Handler.handleCommand chain (ValidateTransaction tx) session 1UL state

    let events, state' = Writer.unwrap result
    
    // Checking that the contract is in not the ACS
    ActiveContractSet.containsContract cHash acs |> should equal false

    //TODO: TBD following

    // Checking that TransactionAddedToMemPool was published

    events |> should haveLength 1
    events |> should contain (EffectsWriter.EventEffect (TransactionAddedToMemPool (txHash, tx)))

    // Checking that the transaction was added to mempool
    MemPool.containsTransaction txHash state'.memoryState.mempool |> should equal true

    // Checking that the transaction was not added to orphans
    OrphanPool.containsTransaction txHash state'.memoryState.orphanPool |> should equal false

[<Test>]
let ``Valid contract should execute``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    
    use session = DatabaseContext.createSession databaseContext
    let account = Account.createRoot ()
    let state = { state with memoryState = { state.memoryState with utxoSet = getSampleUtxoset utxoSet } }
    Account.createActivateContractTransaction account sampleContractCode
    |> Result.map (fun tx ->
        Handler.handleCommand chain (ValidateTransaction tx) session 1UL state
        |> Writer.unwrap)
    |> Result.map (fun (_, state) ->
        ActiveContractSet.containsContract sampleContractHash state.memoryState.activeContractSet
        |> should equal true
        
        TransactionHandler.executeContract sampleInputTx sampleContractHash state.memoryState
        )                
    |> Result.mapError failwith
    |> ignore