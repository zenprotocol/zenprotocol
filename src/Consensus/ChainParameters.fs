module Consensus.ChainParameters

open Consensus.Types

type Chain = 
    | Main
    | Test

let rootPKHash = Hash.compute [| 3uy; 235uy; 227uy; 69uy; 160uy; 193uy; 130uy; 94uy; 110uy; 75uy; 201uy;
                                 131uy; 186uy; 13uy; 173uy; 220uy; 244uy; 192uy; 5uy; 17uy; 204uy; 211uy;
                                 80uy; 60uy; 34uy; 149uy; 101uy; 37uy; 19uy; 1uy; 22uy; 53uy; 147uy|]
    
// Temporary transaction until we will have blocks and test genesis block    
let rootTx= 
    {
        inputs=[];
        outputs=[{lock = PK rootPKHash; spend= {asset = Hash.zero;amount=100000000UL}}]; 
        witnesses=[]
        contract=None
    }
    
let rootTxHash = Transaction.hash rootTx