module Blockchain.Tests.ValidateContractTests

open NUnit.Framework
open FsUnit
open Blockchain
open Consensus
open Wallet
open Messaging.Services.Blockchain
open Messaging.Events
open Infrastructure
open Consensus.Tests.ContractTests
open System.Text

let account = Account.createRoot ()    

// Default initial state of mempool and utxoset    
let utxoSet = UtxoSet.create() |> UtxoSet.handleTransaction ChainParameters.rootTxHash ChainParameters.rootTx                
let mempool = MemPool.create () |> MemPool.add ChainParameters.rootTxHash ChainParameters.rootTx
let orphanPool = OrphanPool.create()       
let acs = ActiveContractSet.create()       

let getBytes (str : string) = Encoding.ASCII.GetBytes str

let getHash = getBytes >> Hash.compute

[<Test>]
let ``Valid contract should be added to ActiveContractSet``() =
    let rootAccount = Account.createRoot ()
    let contractCode = "val test: nat -> nat\nlet test i = i + 1"
    let cHash = getHash contractCode

    let tx = 
        match Account.createContractActivationTransaction rootAccount contractCode with 
            | Ok tx ->
                tx
            | _ ->
                failwith "couldn't get tx"

    let txHash = Transaction.hash tx

    let result = 
        Handler.handleCommand (ValidateTransaction tx) (utxoSet, mempool, orphanPool, acs)        
                    
    let events, (_, mempool, orphanPool, acs) = Writer.unwrap result
    
    // Checking that the contract is in the ACS
    ActiveContractSet.containsContract cHash acs |> should equal true

    // Checking that TransactionAddedToMemPool was published
    events |> should haveLength 1
    events |> should contain (TransactionAddedToMemPool (txHash, tx))

    // Checking that the transaction was added to mempool 
    MemPool.containsTransaction txHash mempool |> should equal true

    // Checking that the transaction was not added to orphans 
    OrphanPool.containsTransaction txHash orphanPool |> should equal false   

[<Test>]
let ``Invalid contract should not be added to ActiveContractSet``() =
    let rootAccount = Account.createRoot ()
    let contractCode = "x"
    let cHash = getHash contractCode

    let tx = 
        match Account.createContractActivationTransaction rootAccount contractCode with 
            | Ok tx -> tx
            | _ -> failwith "couldn't get tx"

    let txHash = Transaction.hash tx

    let result = 
        Handler.handleCommand (ValidateTransaction tx) (utxoSet, mempool, orphanPool, acs)        
                    
    let events, (_, mempool, orphanPool, acs) = Writer.unwrap result
    
    // Checking that the contract is in not the ACS
    ActiveContractSet.containsContract cHash acs |> should equal false

    //TODO: TBD following

    // Checking that TransactionAddedToMemPool was published

    events |> should haveLength 1
    events |> should contain (TransactionAddedToMemPool (txHash, tx))

    // Checking that the transaction was not added to mempool 
    MemPool.containsTransaction txHash mempool |> should equal true

    // Checking that the transaction was added to orphans 
    OrphanPool.containsTransaction txHash orphanPool |> should equal false


[<Test>]
let ``Valid contract should be executed``() =
    Account.createContractActivationTransaction account sampleContractCode
    |> Result.map (fun tx -> 
        Handler.handleCommand (ValidateTransaction tx) (utxoSet, mempool, orphanPool, acs)        
        |> Writer.unwrap)
    |> Result.map (fun (_, (utxoSet, mempool, orphanPool, acs)) -> 
        ActiveContractSet.containsContract sampleContractHash acs
        |> should equal true
        Handler.handleRequest 
            (should equal sampleContractExpectedResult) 
            (ExecuteContract (sampleTxSkeleton, sampleContractHash)) (utxoSet, mempool, orphanPool, acs))
    |> Result.mapError failwith
    |> ignore