module Consensus.Tests.TransactionTests

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit
open Crypto
open TransactionValidation

let shouldEqual expected found =
    try 
        should equal expected found
    with _ as ex ->
        printfn "expected: %A" expected
        printfn "   found: %A" found
        raise ex

let txInMode mode tx =
    match mode with
    | Transaction.Full -> tx
    | Transaction.WithoutWitness -> {tx with witnesses=[]}

let acs = ActiveContractSet.empty

[<Property>]
let ``Transaction serialization round trip produces same result``(mode:Transaction.SerializationMode) (tx:Transaction) =
    tx
    |> Transaction.serialize mode
    |> Transaction.deserialize = Some (txInMode mode tx)

[<Property>]
let ``Different transactions don't produce same serialization result``(mode:Transaction.SerializationMode) (tx1:Transaction) (tx2:Transaction) =
    (txInMode mode tx1 <> txInMode mode tx2) ==> lazy (Transaction.serialize mode tx1 <> Transaction.serialize mode tx2)
    
[<Property>]
let ``Hash size should be 32``(tx:Transaction) =
    let (Hash bytes) = Transaction.hash tx
    Array.length bytes = 32
   
[<Property>]
let ``Different transactions don't produce same hashing result``(tx1:Transaction) (tx2:Transaction) =
    (txInMode Transaction.WithoutWitness tx1 <> txInMode Transaction.WithoutWitness tx2) ==> (Transaction.hash tx1 <> Transaction.hash tx2)

[<Test>]
let ``Transaction should be orphan``() = 
    let input = { 
        txHash = Hash (Array.create 32 0uy)
        index = 0ul 
    }
    let orphanInput = { 
        txHash = Hash (Array.create 32 1uy)
        index = 0ul 
    }
    let output = { lock = Types.Lock.PK (Hash.zero); spend = {asset = Hash.zero; amount = 1UL } }
    let tx = {  
        inputs = [ input; orphanInput ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ input, Unspent output ]
    let txHash = Transaction.hash tx
    validateInputs acs utxos txHash tx |> shouldEqual (Error (Orphan tx) : Result<Transaction, ValidationError>)

[<Test>]
let ``Transaction basic validation should be Ok``() = 
    let input = { 
        txHash = Hash (Array.create 32 0uy)
        index = 0ul 
    }
    let output = { lock = Types.Lock.PK (Hash.zero); spend = {asset = Hash.zero; amount = 1UL } }
    let tx = {  
        inputs = [ input ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    validateBasic tx |> shouldEqual (Ok tx : Result<Transaction, ValidationError>)

[<Property>]
let ``Transaction should have invalid amounts``(utxos:Map<Outpoint, Output>) =
    let fst = Map.toList >> List.map fst
    let snd = Map.toList >> List.map snd
    (utxos |> fst |> List.distinct |> List.length = Map.count utxos && Map.isEmpty utxos |> not) ==> lazy (
        let mutate output = { output with spend = {output.spend with amount = output.spend.amount - 1UL }}
        let tx = { inputs = fst utxos; outputs = (snd >> List.map mutate) utxos; witnesses = []; contract = None }
        let txHash = Transaction.hash tx

        let utxos' = Map.map (fun _ value -> Unspent value) utxos
        validateInputs acs utxos' txHash tx = Error (General "invalid amounts")
    )

[<Property>]
let ``Transaction validation should fail with inputs empty error``(tx:Transaction) =
    validateBasic {tx with inputs = List.empty} = Error (General "inputs empty")

[<Test>]
let ``Transaction validation should fail with outputs invalid error``() =
    let tx = {  
        inputs = 
            [{ 
                txHash = Hash.zero; 
                index = 0ul 
            }];
        witnesses = []
        outputs = 
            [
                { lock = (PK Hash.zero); spend = {asset = Hash.zero; amount = 0UL } }
            ]
        contract = None
    }    
    validateBasic tx |> shouldEqual (Error (General "outputs invalid") : Result<Transaction, ValidationError>)

[<Property>]
let ``Transaction validation should fail with outputs empty error``(tx:Transaction) =
    (tx.inputs.Length <> 0) ==> lazy (validateBasic {tx with outputs = List.empty} = Error (General "outputs empty"))

[<Test>]
let ``Transaction validation should fail with outputs overflow error``() =
    let tx = {  
        inputs = 
            [{ 
                txHash = Hash.zero; 
                index = 0ul 
            }];
        witnesses = []
        outputs = 
            [
                { lock = (PK Hash.zero); spend = {asset = Hash.zero; amount = System.UInt64.MaxValue } };
                { lock = (PK Hash.zero); spend = {asset = Hash.zero; amount = 1UL } }
            ]
        contract = None
    }    
    validateBasic tx |> shouldEqual (Error (General "outputs overflow") : Result<Transaction, ValidationError>)

[<Test>]
let ``Transaction validation should fail with duplicate inputs error``() =
    let input = { 
        txHash = Hash.zero; 
        index = 0ul 
    }
    let tx = {  
        inputs = [ input; input ]
        witnesses = []
        outputs = [ { lock = (PK Hash.zero); spend = {asset = Hash.zero; amount = 1UL } } ]
        contract = None
    }    
    validateBasic tx |> shouldEqual (Error (General "inputs duplicated") : Result<Transaction, ValidationError>)

[<Test>]
let ``Transaction validation should fail with inputs structurally invalid error``() =
    let tx = {  
        inputs =
            [{ 
                txHash = Hash (Array.create 31 0uy); 
                index = 0ul 
            }]
        witnesses = []
        outputs = [ { lock = (PK Hash.zero); spend = {asset = Hash.zero; amount = 1UL } } ]
        contract = None
    }    
    validateBasic tx |> shouldEqual (Error (General "inputs structurally invalid") : Result<Transaction, ValidationError>)

[<Test>]
let ``Signed transaction should be valid``() =
    let keyPair = Crypto.KeyPair.create()
    let _, publicKey = keyPair
    let outputLock = Types.Lock.PK (PublicKey.hash publicKey)
    let input = { 
        txHash = Hash (Array.create 32 0uy)
        index = 0ul 
    }
    let output = { lock = outputLock; spend = {asset = Hash.zero; amount = 1UL } }
    let tx = {  
        inputs = [ input ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let signedTx = Transaction.sign tx [keyPair]
    let utxos = Map.ofSeq [ input, Unspent output ]
    let txHash = Transaction.hash signedTx
    validateInputs acs utxos txHash signedTx |> shouldEqual (Ok signedTx : Result<Transaction, ValidationError>)

[<Test>]
let ``Signed transaction validation result should be invalid witness``() =
    let keyPair = Crypto.KeyPair.create()
    let outputLock = Types.Lock.PK (Hash.zero) // an invalid address
    let input = { 
        txHash = Hash (Array.create 32 0uy)
        index = 0ul 
    }
    let output = { lock = outputLock; spend = {asset = Hash.zero; amount = 1UL } }
    let tx = {  
        inputs = [ input ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let signedTx = Transaction.sign tx [keyPair]
    let utxos = Map.ofSeq [ input, Unspent output ]
    let txHash = Transaction.hash signedTx
    validateInputs acs utxos txHash signedTx |> shouldEqual (Error (General "invalid witness(es)") : Result<Transaction, ValidationError>)