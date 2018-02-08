module Consensus.Tests.UtxoSetTests

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open NUnit.Framework
open FsCheck.NUnit
open FsCheck
open FsUnit

let getUTXO _ = NoOutput

[<Test>]
let ``handling transaction add outputs to set``() = 
    let tx1 = {
        inputs = []
        witnesses = []
        outputs = [{lock = PK Hash.zero; spend = {asset=Hash.zero; amount=1UL}}]
        contract = None
    }    
    let tx1Hash = Transaction.hash tx1
    
    let tx2 = {
        inputs= [{txHash = tx1Hash; index=0ul}]
        witnesses = []
        outputs=[]
        contract = None
    }   
        
    let set = 
        UtxoSet.asDatabase
        |> UtxoSet.handleTransaction getUTXO tx1Hash tx1        
    
    UtxoSet.getUtxosResult getUTXO tx2.inputs set |> should equal (Ok tx1.outputs : Result<_,OutputStatus list>)
        
[<Test>]
let ``handling transaction mark inputs as spent``() = 
    let tx1 = {
        inputs = []
        witnesses = []
        outputs = [{lock = PK Hash.zero; spend = {asset=Hash.zero; amount=1UL}}]
        contract = None
    }    
    let tx1Hash = Transaction.hash tx1
    
    let tx2 = {
        inputs= [{txHash = tx1Hash; index=0ul}]
        witnesses = []
        outputs=[]
        contract = None
    }
    let tx2Hash = Transaction.hash tx2
        
    let set = 
        UtxoSet.asDatabase
        |> UtxoSet.handleTransaction getUTXO tx1Hash tx1   
        |> UtxoSet.handleTransaction getUTXO tx2Hash tx2

         
    (match UtxoSet.getUtxosResult getUTXO tx2.inputs set with
    | Error errors ->
        List.contains Spent errors
    | Ok _ -> false) |> should equal true
        
[<Test>]
let ``Should find Utxo``() =
    let hash = Hash [| 100uy |]
    let outpoint = { txHash = hash; index = 100u }
    let output = { lock = PK hash; spend = { asset = hash; amount = 100UL }}
    let utxos = Map.ofSeq [ (outpoint, Unspent output) ]
    let outputResult = UtxoSet.getUtxos getUTXO [ outpoint ] utxos
    
    match outputResult with 
    | None -> failwith "could not find utxo"
    | Some utxos -> Assert.AreEqual (utxos.[0], output)
    
[<Property>]
let ``Distinct outpoint count should match utxos count``(utxos:Map<Outpoint, Output>) =
    let utxos  = Map.map (fun _ value -> Unspent value) utxos
    let distinctOutpoints = utxos |> Map.toList |> List.map fst

    match UtxoSet.getUtxos getUTXO distinctOutpoints utxos with
    | None -> false
    | Some outputs -> List.length outputs = List.length distinctOutpoints
    
[<Property>]
let ``Should be some utxos``(utxos:Map<Outpoint, Output>) =
    let utxos  = Map.map (fun _ value -> Unspent value) utxos

    Option.isSome <| UtxoSet.getUtxos getUTXO (utxos |> Map.toList |> List.map fst) utxos

[<Property>]
let ``Should be none utxos``(utxos:Map<Outpoint, Output>) (outpoints:List<Outpoint>) =
    let utxos  = Map.map (fun _ value -> Unspent value) utxos

    (outpoints |> List.distinct |> List.length > Map.count utxos) ==> (Option.isNone <| UtxoSet.getUtxos getUTXO outpoints utxos)