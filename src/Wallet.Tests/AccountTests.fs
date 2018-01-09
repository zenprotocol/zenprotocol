module Wallet.Tests.AccountTests

open NUnit.Framework
open FsUnit
open Consensus
open Consensus.ChainParameters
open Consensus.Types
open Wallet

let chain = ChainParameters.Test

let balanceShouldBe asset expected account =     
    let balance = Account.getBalance account
    
    let actual = 
        match Map.tryFind asset balance with
        | Some value -> value
        | None -> 0UL
         
    actual |> should equal expected
    
let anotherAsset = Hash.compute "anotherasset"B    

[<Test>]
let ``received tokens``() =
    let account = Account.create()
    
    let output = {lock = PK account.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}
    
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    
    let account' = Account.handleTransaction (Transaction.hash tx) tx account 
    
    let balances = Account.getBalance account'    
    
    account' |> balanceShouldBe Hash.zero 10UL        

[<Test>]        
let ``tokens spent``() = 
    let account = Account.create()
        
    let output = {lock = PK account.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}
    
    let tx = {inputs=[];outputs=[output;output];witnesses=[];contract=None}
    let txHash = (Transaction.hash tx)
    
    let tx' = {inputs=[{txHash=txHash; index=0ul}];outputs=[];witnesses=[];contract=None}
    
    let account' = 
        Account.handleTransaction txHash tx account 

    let account'' = 
        Account.handleTransaction (Transaction.hash tx') tx' account'        
            
    account' |> balanceShouldBe Hash.zero 20UL            
    account'' |> balanceShouldBe Hash.zero 10UL
                                
[<Test>]
let ``creating, not enough tokens``() =
    let account = Account.create()
    
    let output = {lock = PK account.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}
    
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    
    let account' = Account.handleTransaction (Transaction.hash tx) tx account 

    let result = Account.createTransaction chain account (Account.getAddress account Test) Hash.zero 11UL 
    
    let expected:Result<Transaction,string> = Error "Not enough tokens" 
    
    result |> should equal expected
    
[<Test>]
let ``creating, no change``() = 
    let bob = Account.create ()
    let alice = Account.create ()               
        
    // giving some money to bob
    let output = {lock = PK bob.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}    
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob' = Account.handleTransaction (Transaction.hash tx) tx bob

    // sending money to alice
    let result = Account.createTransaction chain bob' (Account.getAddress alice Test) Hash.zero 10UL 
    
    match result with 
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx -> 
        let alice' = Account.handleTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.handleTransaction (Transaction.hash tx) tx bob
        
        alice' |> balanceShouldBe Hash.zero 10UL
        bob'' |> balanceShouldBe Hash.zero 0UL
        
[<Test>]
let ``creating, with change``() = 
    let bob = Account.create ()
    let alice = Account.create ()                
        
    // giving some money to bob
    let output = {lock = PK bob.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}    
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob' = Account.handleTransaction (Transaction.hash tx) tx bob

    // sending money to alice
    let result = Account.createTransaction chain bob' (Account.getAddress alice Test) Hash.zero 7UL 
    
    match result with 
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx -> 
        let alice' = Account.handleTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.handleTransaction (Transaction.hash tx) tx bob
        
        alice' |> balanceShouldBe Hash.zero 7UL
        bob'' |> balanceShouldBe Hash.zero 3UL
        
[<Test>] 
let ``picking the correct asset``() = 
    let bob = Account.create ()
    let alice = Account.create ()          
        
    // giving some money to bob
    let output = {lock = PK bob.publicKeyHash; spend={asset=anotherAsset;amount=10UL}}
    let output2 = {lock = PK bob.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}    
        
    let tx = {inputs=[];outputs=[output; output2];witnesses=[];contract=None}
    let bob' = Account.handleTransaction (Transaction.hash tx) tx bob
    
    Map.count bob'.outpoints |> should equal 2
    bob' |> balanceShouldBe anotherAsset 10UL

    // sending money to alice
    let result = Account.createTransaction chain bob' (Account.getAddress alice Test) Hash.zero 7UL 
    
    match result with 
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx -> 
        List.length tx.inputs |> should equal 1
    
        let alice' = Account.handleTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.handleTransaction (Transaction.hash tx) tx bob'                        
                
        alice' |> balanceShouldBe Hash.zero 7UL
        bob'' |> balanceShouldBe Hash.zero 3UL 
        
        alice' |> balanceShouldBe anotherAsset 0UL
        bob'' |> balanceShouldBe anotherAsset 10UL

[<Test>] 
let ``picking from multiple inputs``() = 
    let bob = Account.create ()
    let alice = Account.create ()               
        
    // giving some money to bob
    let output = {lock = PK bob.publicKeyHash; spend={asset=Hash.zero;amount=5UL}}
    let output2 = {lock = PK bob.publicKeyHash; spend={asset=Hash.zero;amount=7UL}}    
        
    let tx = {inputs=[];outputs=[output; output2];witnesses=[];contract=None}
    let bob' = Account.handleTransaction (Transaction.hash tx) tx bob    

    // sending money to alice
    let result = Account.createTransaction chain bob' (Account.getAddress alice Test) Hash.zero 10UL 
    
    match result with 
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->             
        let alice' = Account.handleTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.handleTransaction (Transaction.hash tx) tx bob'                        
                
        alice' |> balanceShouldBe Hash.zero 10UL
        bob'' |> balanceShouldBe Hash.zero 2UL