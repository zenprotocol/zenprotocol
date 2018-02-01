module Wallet.Tests.AccountTests

open NUnit.Framework
open FsUnit
open Consensus
open Consensus.ChainParameters
open Consensus.Types
open Wallet
open TestsInfrastructure.Constraints

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
    
    let account' = Account.addTransaction (Transaction.hash tx) tx account 
    
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
        Account.addTransaction txHash tx account 

    let account'' = 
        Account.addTransaction (Transaction.hash tx') tx' account'        
            
    account' |> balanceShouldBe Hash.zero 20UL            
    account'' |> balanceShouldBe Hash.zero 10UL
                                
[<Test>]
let ``creating, not enough tokens``() =
    let account = Account.create()
    
    let output = {lock = PK account.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}
    
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    
    let account' = Account.addTransaction (Transaction.hash tx) tx account 

    let result = Account.createTransaction chain account account.publicKeyHash { asset = Hash.zero; amount = 11UL }
    
    let expected:Result<Transaction,string> = Error "Not enough tokens" 
    
    result |> should equal expected
    
[<Test>]
let ``creating, no change``() = 
    let bob = Account.create ()
    let alice = Account.create ()               
        
    // giving some money to bob
    let output = {lock = PK bob.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}    
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob

    // sending money to alice
    let result = Account.createTransaction chain bob' alice.publicKeyHash { asset = Hash.zero; amount = 10UL }
    
    match result with 
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx -> 
        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob
        
        alice' |> balanceShouldBe Hash.zero 10UL
        bob'' |> balanceShouldBe Hash.zero 0UL
        
[<Test>]
let ``creating, with change``() = 
    let bob = Account.create ()
    let alice = Account.create ()                
        
    // giving some money to bob
    let output = {lock = PK bob.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}    
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob

    // sending money to alice
    let result = Account.createTransaction chain bob' alice.publicKeyHash { asset = Hash.zero; amount = 7UL }
    
    match result with 
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx -> 
        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob
        
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
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob
    
    Account.getUnspentOutputs bob' |> should haveCount 2
    bob' |> balanceShouldBe anotherAsset 10UL

    // sending money to alice
    let result = Account.createTransaction chain bob' alice.publicKeyHash { asset = Hash.zero; amount = 7UL } 
    
    match result with 
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx -> 
        List.length tx.inputs |> should equal 1
    
        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob'                        
                
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
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob    

    // sending money to alice
    let result = Account.createTransaction chain bob' alice.publicKeyHash { asset = Hash.zero; amount = 10UL }
    
    match result with 
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->             
        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob'                        
                
        alice' |> balanceShouldBe Hash.zero 10UL
        bob'' |> balanceShouldBe Hash.zero 2UL
        
[<Test>]
let ``create execute contract transaction``() = 
    let account = Account.createRoot ()
    
    let executeContract _ _ txSkeleton =             
        let tx = 
            txSkeleton
            |> TxSkeleton.addOutput {lock=Contract Hash.zero;spend={asset=Hash.zero;amount=1UL}} 
            |> Transaction.fromTxSkeleton 

        tx |> Messaging.Services.TransactionResult.Ok
    
    let spends = Map.add Hash.zero 1UL Map.empty
    
    let result = Account.createExecuteContractTransaction account executeContract Hash.zero "" spends
    
    result |> should be ok
    
    let tx = 
        match result with
        | Ok tx -> tx
        | Error error -> failwith error
    
    tx.inputs |> should haveLength 1    
    
    
[<Test>]
let ``account sync up``() =        
    let startBlockHeader = {
        version = 0ul
        parent = ChainParameters.getGenesisHash Chain.Test
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }
    
    let account = 
        {Account.create () with tip = BlockHeader.hash startBlockHeader}
    
    let output = {lock = PK account.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}        
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    let txHash = Transaction.hash tx
        
    let account = Account.addTransaction txHash tx account
                
    List.exists (fst >> (=) txHash) account.mempool |> should equal true             
                
    let header = {
        version = 0ul
        parent = BlockHeader.hash startBlockHeader
        blockNumber = 3ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }
            
    let block = {
        header = header
        transactions = [tx]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot= Hash.zero
        commitments= []
    }
    
    let blockHash = Block.hash block
    
    let account' = Account.sync Chain.Test blockHash (fun _ -> startBlockHeader) (fun _ -> block) account 
    
    account'.tip |> should equal blockHash
    account'.mempool |> should haveLength 0
    
[<Test>]
let ``sync up from empty wallet``() =             
    let account = Account.create () 
            
    let output = {lock = PK account.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}        
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
         
    let header = {
        version = 0ul
        parent = ChainParameters.getGenesisHash Chain.Test
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }
            
    let block = {
        header = header
        transactions = [tx]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot= Hash.zero
        commitments = []
    }
    
    let blockHash = Block.hash block
    
    let account = Account.sync Chain.Test blockHash (fun _ -> failwith "unexpected") 
                    (fun blockHash ->
                        if blockHash = ChainParameters.getGenesisHash Chain.Test then 
                            Block.createGenesis chain [Transaction.rootTx] (0UL,0UL)
                        else
                            block) account 
    
    account.tip |> should equal blockHash
    account.mempool |> should haveLength 0
    Account.getUnspentOutputs account |> should haveCount 1    
    
[<Test>]
let ``account reorg``() = 
    let startBlockHeader = {
        version = 0ul
        parent = ChainParameters.getGenesisHash Chain.Test
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }
    
    let account = 
        {Account.create () with tip = BlockHeader.hash startBlockHeader}
    
    let output = {lock = PK account.publicKeyHash; spend={asset=Hash.zero;amount=10UL}}        
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    let txHash = Transaction.hash tx
        
    let account = Account.addTransaction txHash tx account
    
    let header = {
        version = 0ul
        parent = BlockHeader.hash startBlockHeader
        blockNumber = 3ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }
            
    let block = {
        header = header
        transactions = [tx]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot= Hash.zero
        commitments= []
    }
    
    let blockHash = Block.hash block
    
    let account = Account.sync Chain.Test blockHash (fun _ -> startBlockHeader) (fun _ -> block) account 
    account.tip |> should equal blockHash
    
    let header2 = {
       version = 0ul
       parent = BlockHeader.hash startBlockHeader
       blockNumber = 3ul
       commitments = Hash.zero
       timestamp = 0UL
       difficulty = 0ul
       nonce = 1UL,0UL
    }
               
    let block2 = {
       header = header2
       transactions = []
       txMerkleRoot = Hash.zero
       witnessMerkleRoot = Hash.zero
       activeContractSetMerkleRoot= Hash.zero
       commitments= []
    }
    
    let blockHash2 = Block.hash block2
    
    let account = Account.sync Chain.Test blockHash2 (fun _ -> block.header) 
                        (fun bh -> 
                            if bh = blockHash then
                                block
                            else if bh = blockHash2 then
                                block2
                            else 
                                failwithf "unexpected %A" bh) account
        
    account.tip |> should equal blockHash2
    account.mempool |> should haveLength 1
    List.exists (fst >> (=) txHash) account.mempool |> should equal true             
    
                
        
 
        
    
    
                  
        