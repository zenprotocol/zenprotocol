module Blockchain.Tests.ValidateTransactionTests

// This tests only for ValidateTransaction command
// We don't need to cover the entire outcome of transaction validation
// we have transaction validation tests for that.
// We are checking that all events are raised, tx enter mempool, utxoset or orphan tx list

open Xunit
open FsUnit.Xunit
open Blockchain
open Consensus
open Consensus.Types
open Wallet
open Messaging.Services.Blockchain
open Messaging.Events
open Infrastructure


let address = "tz1q9xcl80v3mqqpux4s3g09cezwqlk5gfrn3u8x8pn4rn7zg65ek5yqrngkp2"

let tx = 
    let account = Account.createRoot ()
    match Account.createTransaction account address Hash.zero 1UL with
    | Ok tx -> tx
    | Error error -> failwith error
let txHash = Transaction.hash tx    
let txOutpoints = List.mapi (fun i _ -> {txHash=txHash;index= uint32 i}) tx.outputs    
    
let shouldBeenRaised (events:Event list) event = 
    List.find ((=) event) events |> ignore

[<Fact>]
let ``valid transaction raise events and update state``() =
    let utxoSet = 
        UtxoSet.create() 
        |> UtxoSet.handleTransaction ChainParameters.rootTxHash ChainParameters.rootTx
                
    let mempool = 
        MemPool.create ()
        |> MemPool.add ChainParameters.rootTxHash ChainParameters.rootTx
                   
    let result = Handler.handleCommand (ValidateTransaction tx) (utxoSet, mempool)
    
    let events, (utxoSet', mempool') = Writer.unwrap result
        
    // Checking that the event raised
    TransactionAddedToMemPool (txHash, tx)
    |> shouldBeenRaised events
    
    // Checking only 1 event raised
    List.length events |> should equal 1
    
    // Checking the tx is in the mempool
    MemPool.containsTransaction txHash mempool' |> should equal true 
    
    // Checking the tx is in the utxoset
    UtxoSet.getUtxos txOutpoints utxoSet' |> should equal (Some tx.outputs) 

[<Fact>]
let ``Invalid tx doesn't raise events or update state``() =
    // the only difference from the valid test is that the mempool and utxoset do not have the 
    // root tx
    
    let utxoSet = UtxoSet.create() 
                        
    let mempool = MemPool.create ()        
                   
    let result = Handler.handleCommand (ValidateTransaction tx) (utxoSet, mempool)
    
    let events, (utxoSet', mempool') = Writer.unwrap result
    
    // Checking no events raised
    List.length events |> should equal 0
    
    // Checking the tx is not in the mempool
    MemPool.containsTransaction txHash mempool' |> should equal false 
    
    // Checking the tx is not in the utxoset
    UtxoSet.getUtxos txOutpoints utxoSet' |> should equal None 
    
[<Fact>]
let ``tx already in mempool nothing happen`` () = 
    let utxoSet = 
        UtxoSet.create() 
        |> UtxoSet.handleTransaction ChainParameters.rootTxHash ChainParameters.rootTx
                
    let mempool = 
        MemPool.create ()
        |> MemPool.add ChainParameters.rootTxHash ChainParameters.rootTx
                   
    let result = Handler.handleCommand (ValidateTransaction ChainParameters.rootTx) (utxoSet, mempool)
    
    let events, (utxoSet', mempool') = Writer.unwrap result
    
    // Checking no events raised
    List.length events |> should equal 0        
    
    // Checking that state didn't change
    utxoSet' |> should equal utxoSet    
    mempool' |> should equal mempool 
          