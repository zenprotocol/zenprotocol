module Consensus.Tests.ContractWalletsTests

open Consensus
open Consensus.ContractWallets
open Consensus.Types
open NUnit.Framework
open FsUnit

let getWallet _ = Map.empty        

[<Test>]
let ``handle transaction that doesnt affect any contracts``() =    
    let tx = 
        {
            inputs=[{txHash=Hash.zero;index=0ul}]
            outputs=[{lock=PK Hash.zero;spend={amount=1UL;asset=Hash.zero}}]
            witnesses=[]
            contract=None
        }
    let inputs = [{lock=PK Hash.zero;spend={amount=1UL;asset=Hash.zero}}]        
    let txHash = Transaction.hash tx
        
    let wallets = 
        handleTransaction getWallet ContractWallets.asDatabase txHash tx inputs     
    
    wallets |> should haveCount 0
    
[<Test>]
let ``handle transaction with new contract output``() =
    let cHash = Hash.compute "test"B
    let output = {lock=Contract cHash;spend={amount=1UL;asset=Hash.zero}}
         
    let tx = 
        {
            inputs=[{txHash=Hash.zero;index=0ul}]
            outputs=[output]
            witnesses=[]
            contract=None
        }
    let inputs = [{lock=PK Hash.zero;spend={amount=1UL;asset=Hash.zero}}]        
    let txHash = Transaction.hash tx
    let outpoint = {txHash=txHash;index=0ul}  
        
    let wallets = 
        handleTransaction getWallet ContractWallets.asDatabase txHash tx inputs     
    
    wallets |> should haveCount 1      
    
    let expectedWallet : ContractWallet = 
        Map.empty
        |> Map.add Hash.zero [(outpoint,output)]
    
    get getWallet cHash wallets |> should equal expectedWallet
    
[<Test>]
let ``handle transaction with new contract output which is added to existing wallet``() =
    let cHash = Hash.compute "test"B
    let output = {lock=Contract cHash;spend={amount=1UL;asset=Hash.zero}}
         
    let tx = 
        {
            inputs=[{txHash=Hash.zero;index=0ul}]
            outputs=[output]
            witnesses=[]
            contract=None
        }
    let inputs = [{lock=PK Hash.zero;spend={amount=1UL;asset=Hash.zero}}]        
    let txHash = Transaction.hash tx
    let outpoint = {txHash=txHash;index=0ul}  
    
    let existingContractOutput = ({txHash=Hash.compute "some"B;index=0ul},{lock=Contract cHash;spend={amount=2UL;asset=Hash.zero}})
    
    let wallet = 
         Map.empty
         |> Map.add Hash.zero [existingContractOutput]
    
    let wallets = 
        Map.add cHash wallet Map.empty
        
    let wallets = 
        handleTransaction getWallet wallets txHash tx inputs     
    
    wallets |> should haveCount 1      
    
    let expectedWallet : ContractWallet = 
        Map.empty
        |> Map.add Hash.zero [existingContractOutput;(outpoint,output)]        
    
    get getWallet cHash wallets |> should equal expectedWallet    
    
[<Test>]
let ``handle transaction with new contract output which is added to existing wallet at the bottom``() =
    let cHash = Hash.compute "test"B
    let output = {lock=Contract cHash;spend={amount=2UL;asset=Hash.zero}}
         
    let tx = 
        {
            inputs=[{txHash=Hash.zero;index=0ul}]
            outputs=[output]
            witnesses=[]
            contract=None
        }
    let inputs = [{lock=PK Hash.zero;spend={amount=1UL;asset=Hash.zero}}]        
    let txHash = Transaction.hash tx
    let outpoint = {txHash=txHash;index=0ul}  
    
    let existingContractOutput = ({txHash=Hash.compute "some"B;index=0ul},{lock=Contract cHash;spend={amount=1UL;asset=Hash.zero}})
    
    let wallet = 
         Map.empty
         |> Map.add Hash.zero [existingContractOutput]
    
    let wallets = 
        Map.add cHash wallet Map.empty
        
    let wallets = 
        handleTransaction getWallet wallets txHash tx inputs     
    
    wallets |> should haveCount 1      
    
    let expectedWallet : ContractWallet = 
        Map.empty
        |> Map.add Hash.zero [(outpoint,output);existingContractOutput]        
    
    get getWallet cHash wallets |> should equal expectedWallet            
    
[<Test>]
let ``handle transaction which spent contract output``() =
    let cHash = Hash.compute "test"B
       
    let extraOutput = ({txHash=Hash.compute "some2"B;index=0ul},{lock=Contract cHash;spend={amount=1UL;asset=Hash.zero}})
                        
    let outpoint = {txHash=Hash.compute "some"B;index=0ul}
    let output = {lock=Contract cHash;spend={amount=1UL;asset=Hash.zero}}
         
    let tx = 
        {
            inputs=[outpoint]
            outputs=[]
            witnesses=[]
            contract=None
        }
    let inputs = [output]        
    let txHash = Transaction.hash tx
            
    let wallet = 
         Map.empty
         |> Map.add Hash.zero [(outpoint,output);extraOutput]
    
    let wallets = 
        Map.add cHash wallet Map.empty
        
    let wallets = 
        handleTransaction getWallet wallets txHash tx inputs     
    
    wallets |> should haveCount 1      
    
    let expectedWallet : ContractWallet = 
        Map.empty
        |> Map.add Hash.zero [extraOutput]
                      
    get getWallet cHash wallets |> should equal expectedWallet  
    
[<Test>]
let ``handle transaction which spent last contract output``() =
    let cHash = Hash.compute "test"B
                           
    let outpoint = {txHash=Hash.compute "some"B;index=0ul}
    let output = {lock=Contract cHash;spend={amount=1UL;asset=Hash.zero}}
         
    let tx = 
        {
            inputs=[outpoint]
            outputs=[]
            witnesses=[]
            contract=None
        }
    let inputs = [output]        
    let txHash = Transaction.hash tx
            
    let wallet = 
         Map.empty
         |> Map.add Hash.zero [(outpoint,output)]
    
    let wallets = 
        Map.add cHash wallet Map.empty
        
    let wallets = 
        handleTransaction getWallet wallets txHash tx inputs     
    
    wallets |> should haveCount 1      
    
    let expectedWallet : ContractWallet = 
        Map.empty
                      
    get getWallet cHash wallets |> should equal expectedWallet            
    
[<Test>]          
let ``undo block should return stuff to the wallet``() =
    let cHash = Hash.compute "test"B
                                
    let outpoint = {txHash=Hash.compute "some"B;index=0ul}
    let output = {lock=Contract cHash;spend={amount=1UL;asset=Hash.zero}}
    
    let getOutput _ = output
    
    let tx = 
        {
            inputs=[outpoint]
            outputs=[output]
            witnesses=[]
            contract=None
        }
    let txHash = Transaction.hash tx
                
    let block = 
        {
            header=
                {
                    version=0ul;
                    parent= Hash.zero;
                    blockNumber=2ul;
                    commitments=Hash.zero;
                    timestamp= 0UL;
                    difficulty= 0ul;
                    nonce= 0UL,0UL;
                }
            txMerkleRoot=Hash.zero
            witnessMerkleRoot=Hash.zero
            activeContractSetMerkleRoot=Hash.zero
            commitments=[]
            transactions=[tx]
        }        
    
    let wallet = 
        Map.empty
        |> Map.add Hash.zero [{txHash=txHash;index=0ul;},output]

    let wallets =
        Map.add cHash wallet Map.empty
        |> undoBlock getOutput getWallet block     
        
    wallets |> should haveCount 1      
    
    let expectedWallet : ContractWallet = 
        Map.empty
        |> Map.add Hash.zero [(outpoint,output)]
                      
    get getWallet cHash wallets |> should equal expectedWallet         
                  

            