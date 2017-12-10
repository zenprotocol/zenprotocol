module Consensus.Tests.TransactionTests

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open FsCheck.Xunit
open FsCheck

let txInMode mode tx =
    match mode with
    | Transaction.Full -> tx
    | Transaction.WithoutWitness -> {tx with witnesses=[]}

[<Property>]
let ``Transaction serialization round trip produces same result``(mode:Transaction.SerializationMode) (tx:Transaction) =
    tx
    |> Transaction.serialize mode
    |> Transaction.deserialize = txInMode mode tx

[<Property>]
let ``Different transactions don't produce same serialization result``(mode:Transaction.SerializationMode) (tx1:Transaction) (tx2:Transaction) =
    (txInMode mode tx1 <> txInMode mode tx2) ==> (Transaction.serialize mode tx1 <> Transaction.serialize mode tx2)
    
[<Property>]
let ``Hash size should be 32``(tx:Transaction) =
    let (Hash bytes) = Transaction.hash tx
    Array.length bytes = 32
   
[<Property>]
let ``Different transactions don't produce same hashing result``(tx1:Transaction) (tx2:Transaction) =
    (txInMode Transaction.WithoutWitness tx1 <> txInMode Transaction.WithoutWitness tx2) ==> (Transaction.hash tx1 <> Transaction.hash tx2)

[<Property>]
let ``Transaction should be orphan``(tx:Transaction) (utxos:Map<Outpoint, Output>) =
    let utxos  = Map.map (fun key value -> Unspent value) utxos

    
    (tx.inputs |> List.distinct |> List.length > Map.count utxos) ==> (Transaction.validate utxos tx = Error "orphan")

[<Property>]
let ``Transaction should not be orphan``(tx:Transaction) =
    let emptyHash = Hash [||]
    let outputs = List.map (fun _ -> Unspent { lock = PK emptyHash; spend = { asset = emptyHash; amount = 1UL }}) tx.inputs
    let utxos = Map.ofList (List.zip tx.inputs outputs) 
    Transaction.validate utxos tx <> Error "orphan"

[<Property>]
let ``Transaction should be valid``(utxos:Map<Outpoint, Output>) =
    let utxos'  = Map.map (fun key value -> Unspent value) utxos
    let fst = Map.toList >> List.map fst
    let snd = Map.toList >> List.map snd
    (utxos |> fst |> List.distinct |> List.length = Map.count utxos) ==>
    let tx = { inputs = fst utxos; outputs = snd utxos; witnesses = [] }
    (Transaction.validate utxos' tx = Ok tx)
        
[<Property>]
let ``Transaction should have invalid amounts``(utxos:Map<Outpoint, Output>) =
    let utxos'  = Map.map (fun key value -> Unspent value) utxos

    let fst = Map.toList >> List.map fst
    let snd = Map.toList >> List.map snd
    (utxos |> fst |> List.distinct |> List.length = Map.count utxos && Map.count utxos > 0) ==>
    let mutate output = { output with spend = {output.spend with amount = output.spend.amount - 1UL }}
    let tx = { inputs = fst utxos; outputs = (snd >> List.map mutate) utxos; witnesses = [] }
    (Transaction.validate utxos' tx = Error "invalid amounts")