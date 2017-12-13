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

// Helper functions for the tests
let getAddress a = Account.getAddress a ChainParameters.Test
let createTransaction address amount account = 
    match Account.createTransaction account address Hash.zero amount with
    | Ok tx -> tx
    | Error error -> failwith error
let getTxOutpints txHash tx = List.mapi (fun i _ -> {txHash=txHash;index= uint32 i}) tx.outputs    
let areOutpointsInSet outpoints set = 
    match UtxoSet.getUtxos outpoints set with
    | Some _ -> true
    | None -> false
 
// Some default transaction to work with during the tests
let tx = 
    let account = Account.createRoot ()    
    createTransaction (getAddress account) 1UL account    
let txHash = Transaction.hash tx    
let txOutpoints = getTxOutpints txHash tx     
    
// Default initial state of mempool and utxoset    
let utxoSet = UtxoSet.create() |> UtxoSet.handleTransaction ChainParameters.rootTxHash ChainParameters.rootTx                
let mempool = MemPool.create () |> MemPool.add ChainParameters.rootTxHash ChainParameters.rootTx
let orphanPool = OrphanPool.create()       

[<Test>]
let ``valid transaction raise events and update state``() =                                          
    let result = Handler.handleCommand (ValidateTransaction tx) (utxoSet, mempool, OrphanPool.create())
    
    let events, (utxoSet', mempool', _) = Writer.unwrap result
        
    // Checking that the event raised
    events |> should contain (TransactionAddedToMemPool (txHash, tx))    
    
    // Checking only 1 event raised
    events |> should haveLength 1
    
    // Checking the tx is in the mempool
    MemPool.containsTransaction txHash mempool' |> should equal true 
    
    // Checking the tx is in the utxoset
    UtxoSet.getUtxos txOutpoints utxoSet' |> should equal (Some tx.outputs) 

[<Test>]
let ``Invalid tx doesn't raise events or update state``() = 
    let invalidTx = {inputs=[];outputs=[];witnesses=[];}   
                   
    let result = Handler.handleCommand (ValidateTransaction invalidTx) (utxoSet, mempool, OrphanPool.create())
    
    let events, (utxoSet', mempool', orphanPool') = Writer.unwrap result
    
    // Checking no events raised
    events |> should haveLength 0
    
    // Checking the tx is not in the mempool
    MemPool.containsTransaction txHash mempool' |> should equal false 
    
    // Checking the tx is not in the utxoset
    UtxoSet.getUtxos txOutpoints utxoSet' |> should equal None
    
    // Orphan should be empty
    Map.isEmpty orphanPool' |> should equal true 
    
[<Test>]
let ``tx already in mempool nothing happen`` () =                       
    let result = Handler.handleCommand (ValidateTransaction ChainParameters.rootTx) (utxoSet, mempool, OrphanPool.create())
    
    let events, (utxoSet', mempool', _) = Writer.unwrap result
    
    // Checking no events raised
    events |> should haveLength 0
    
    // Checking that state didn't change
    utxoSet' |> should equal utxoSet    
    mempool' |> should equal mempool 

[<Test>]
let ``orphan tx added to orphan list``() =
    let utxoSet = UtxoSet.create()
    let mempool = MemPool.create ()
                   
    let result = Handler.handleCommand (ValidateTransaction tx) (utxoSet, mempool, orphanPool)
    
    let events, (utxoSet', mempool', orphanPool') = Writer.unwrap result
    
    // Checking no events raised
    events |> should haveLength 0
   
    // Checking the tx is not in the mempool
    MemPool.containsTransaction txHash mempool' |> should equal false 
    
    // Checking the tx is not in the utxoset
    UtxoSet.getUtxos txOutpoints utxoSet' |> should equal None 
    
    OrphanPool.containsTransaction txHash orphanPool' |> should equal true
    
[<Test>]
let ``origin tx hit mempool, orphan tx should be added to mempool``() =                                                       
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let account2 = Account.create ()
        
    let tx1 = createTransaction (getAddress account1) 1UL rootAccount
    let tx1Hash = Transaction.hash tx1
    
    let tx2 = 
        Account.handleTransaction tx1Hash tx1 account1
        |> createTransaction (getAddress account2) 1UL 
    let tx2Hash = Transaction.hash tx2             
    
    // Sending orphan transaction first, which should be added to orphan list
    let result = Handler.handleCommand (ValidateTransaction tx2) (utxoSet, mempool, orphanPool)    
    let events, (utxoSet', mempool', orphanPool') = Writer.unwrap result
    
    // Checking that the transaction is only in the mempool and no event were raised
    events |> should haveLength 0
    MemPool.containsTransaction tx2Hash mempool' |> should equal false     
    UtxoSet.getUtxos (getTxOutpints tx2Hash tx2) utxoSet' |> should equal None
    OrphanPool.containsTransaction tx2Hash orphanPool' |> should equal true

    // Sending origin, which should cause both transaction to be added to the mempool
    let result' = Handler.handleCommand (ValidateTransaction tx1) (utxoSet', mempool', orphanPool')    
    let events', (utxoSet'', mempool'', orphanPool'') = Writer.unwrap result'
    
    // Checking that both transaction added to mempool and published
    events' |> should haveLength 2 
    events' |> should contain (TransactionAddedToMemPool (tx1Hash,tx1))    
    events' |> should contain (TransactionAddedToMemPool (tx2Hash,tx2))
    MemPool.containsTransaction tx1Hash mempool'' |> should equal true     
    MemPool.containsTransaction tx2Hash mempool'' |> should equal true
    UtxoSet.getUtxos (getTxOutpints tx1Hash tx1) utxoSet'' |> should equal None // Was already spent by tx2
    UtxoSet.getUtxos (getTxOutpints tx2Hash tx2) utxoSet'' |> should equal (Some tx2.outputs)
    OrphanPool.containsTransaction tx2Hash orphanPool'' |> should equal false
    OrphanPool.containsTransaction tx1Hash orphanPool'' |> should equal false
    
[<Test>]    
let ``orphan transaction is eventually invalid``() =
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let account2 = Account.create ()
           
    let tx1 = createTransaction (getAddress account1) 2UL rootAccount
    let tx1Hash = Transaction.hash tx1
    
    let tx2 = 
        let tx = 
            Account.handleTransaction tx1Hash tx1 account1
            |> createTransaction (getAddress account2) 2UL
        // let's change one of the outputs value and reassign to make invalid tx
        let output = tx.outputs.[0]
        let output' = {output with spend = {amount = output.spend.amount - 1UL; asset = output.spend.asset}}
        let outputs = output' :: List.tail tx.outputs
        let tx' = { tx with outputs = outputs}
        Transaction.sign tx' [account1.keyPair]                              
    let tx2Hash = Transaction.hash tx2
    
    // Sending orphan transaction first, which should be added to orphan list
    let result = Handler.handleCommand (ValidateTransaction tx2) (utxoSet, mempool, orphanPool)    
    let events, (utxoSet', mempool', orphanPool') = Writer.unwrap result
    
    // Checking that the transaction is only in the mempool and no event were raised
    events |> should haveLength 0
    MemPool.containsTransaction tx2Hash mempool' |> should equal false     
    UtxoSet.getUtxos (getTxOutpints tx2Hash tx2) utxoSet' |> should equal None
    OrphanPool.containsTransaction tx2Hash orphanPool' |> should equal true
    
     // Sending origin, which should cause orphan transaction to be rejected and removed from orphan list
    let result' = Handler.handleCommand (ValidateTransaction tx1) (utxoSet', mempool', orphanPool')    
    let events', (utxoSet'', mempool'', orphanPool'') = Writer.unwrap result'
    
    // Checking that the origin tx is published and orphan removed as invalid
    events' |> should haveLength 1
    events' |> should contain (TransactionAddedToMemPool (tx1Hash,tx1))        
    MemPool.containsTransaction tx1Hash mempool'' |> should equal true     
    MemPool.containsTransaction tx2Hash mempool'' |> should equal false
    UtxoSet.getUtxos (getTxOutpints tx1Hash tx1) utxoSet'' |> should equal (Some tx1.outputs) 
    UtxoSet.getUtxos (getTxOutpints tx2Hash tx2) utxoSet'' |> should equal None
    OrphanPool.containsTransaction tx2Hash orphanPool'' |> should equal false
    OrphanPool.containsTransaction tx1Hash orphanPool'' |> should equal false
    
[<Test>]
let ``two orphan transaction spending same input``() =
    let rootAccount = Account.createRoot ()
    let account1 = Account.create ()
    let account2 = Account.create ()
    let account3 = Account.create ()
        
    let tx1 = createTransaction (getAddress account1) 1UL rootAccount
    let tx1Hash = Transaction.hash tx1
    
    let tx2 = 
        Account.handleTransaction tx1Hash tx1 account1
        |> createTransaction (getAddress account2) 1UL 
    let tx2Hash = Transaction.hash tx2
    
    let tx3 = 
        Account.handleTransaction tx1Hash tx1 account1
        |> createTransaction (getAddress account3) 1UL
    let tx3Hash = Transaction.hash tx3              
    
    let (>>=) = Writer.bind
    
    // Sending both tx2 and tx3, both should be added to orphan pool
    let result = 
        Handler.handleCommand (ValidateTransaction tx2) (utxoSet, mempool, orphanPool)        
        >>= 
        Handler.handleCommand (ValidateTransaction tx3)
                    
    let events, (utxoSet', mempool', orphanPool') = Writer.unwrap result
    
    // Checking that the transaction is only in the mempool and no event were raised
    events |> should haveLength 0
    MemPool.containsTransaction tx2Hash mempool' |> should equal false     
    MemPool.containsTransaction tx3Hash mempool' |> should equal false
    UtxoSet.getUtxos (getTxOutpints tx2Hash tx2) utxoSet' |> should equal None
    UtxoSet.getUtxos (getTxOutpints tx3Hash tx3) utxoSet' |> should equal None
    OrphanPool.containsTransaction tx2Hash orphanPool' |> should equal true
    OrphanPool.containsTransaction tx3Hash orphanPool' |> should equal true

    // Sending origin, which should pick one of the transactions (we cannot know which one, for now at least)
    let result' = Handler.handleCommand (ValidateTransaction tx1) (utxoSet', mempool', orphanPool')
    let events', (utxoSet'', mempool'', orphanPool'') = Writer.unwrap result'
    
    // Checking that both transaction added to mempool and published
    events' |> should haveLength 2
    events' |> should contain (TransactionAddedToMemPool (tx1Hash,tx1))
    MemPool.containsTransaction tx1Hash mempool'' |> should equal true
    MemPool.containsTransaction tx2Hash mempool'' <> MemPool.containsTransaction tx3Hash mempool''
    |> should equal true

    UtxoSet.getUtxos (getTxOutpints tx1Hash tx1) utxoSet'' |> should equal None // Was already spent by tx2
    areOutpointsInSet (getTxOutpints tx2Hash tx2) utxoSet'' <> areOutpointsInSet (getTxOutpints tx3Hash tx3) utxoSet'' 
    |> should equal true

    OrphanPool.containsTransaction tx3Hash orphanPool'' |> should equal false
    OrphanPool.containsTransaction tx2Hash orphanPool'' |> should equal false
    OrphanPool.containsTransaction tx1Hash orphanPool'' |> should equal false