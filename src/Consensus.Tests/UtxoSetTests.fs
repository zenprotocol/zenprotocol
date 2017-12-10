module Consensus.Tests.UtxoSetTests

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open FsCheck.Xunit
open FsCheck
open FsUnit.Xunit

[<Xunit.Fact>]
let ``handling transaction add outputs to set``() = 
    let tx1 = {
        inputs = [];
        witnesses = [];
        outputs = [{lock = (PK Hash.zero); spend = {asset=Hash.zero; amount=1UL}}]          
    }    
    let tx1Hash = Transaction.hash tx1
    
    let tx2 = {
        inputs= [{txHash = tx1Hash; index=0ul}];
        witnesses = [];
        outputs=[];
    }   
        
    let set = 
        UtxoSet.create ()
        |> UtxoSet.handleTransaction tx1Hash tx1        
    
    UtxoSet.isSomeSpent tx2.inputs set |> should equal false
    UtxoSet.getUtxos tx2.inputs set |> should equal (Some tx1.outputs)
        
[<Xunit.Fact>]
let ``handling transaction mark inputs as spent``() = 
    let tx1 = {
        inputs = [];
        witnesses = [];
        outputs = [{lock = (PK Hash.zero); spend = {asset=Hash.zero; amount=1UL}}]          
    }    
    let tx1Hash = Transaction.hash tx1
    
    let tx2 = {
        inputs= [{txHash = tx1Hash; index=0ul}];
        witnesses = [];
        outputs=[];
    }
    let tx2Hash = Transaction.hash tx2
        
    let set = 
        UtxoSet.create ()
        |> UtxoSet.handleTransaction tx1Hash tx1   
        |> UtxoSet.handleTransaction tx2Hash tx2     
    
    UtxoSet.getUtxos tx2.inputs set |> should equal None
    UtxoSet.isSomeSpent tx2.inputs set |> should equal true
        
        
[<Xunit.Fact>]
let ``Should find Utxo``() =
    let hash = Hash [| 100uy |]
    let outpoint = { txHash = hash; index = 100u }
    let output = { lock = PK hash; spend = { asset = hash; amount = 100UL }}
    let utxos = Map.ofSeq [ (outpoint, Unspent output) ]
    let outputResult = UtxoSet.getUtxos [ outpoint ] utxos
    
    match outputResult with 
    | None -> failwith ""
    | Some utxos -> Xunit.Assert.Equal (utxos.[0], output)
    
[<Property>]
let ``Distinct outpoint count should match utxos count``(utxos:Map<Outpoint, Output>) =
    let utxos  = Map.map (fun key value -> Unspent value) utxos
    let distinctOutpoints = utxos |> Map.toList |> List.map fst

    match UtxoSet.getUtxos distinctOutpoints utxos with
    | None -> false
    | Some outputs -> List.length outputs = List.length distinctOutpoints
    
[<Property>]
let ``Should be some utxos``(utxos:Map<Outpoint, Output>) =
    let utxos  = Map.map (fun key value -> Unspent value) utxos

    Option.isSome <| UtxoSet.getUtxos (utxos |> Map.toList |> List.map fst) utxos

[<Property>]
let ``Should be none utxos``(utxos:Map<Outpoint, Output>) (outpoints:List<Outpoint>) =
    let utxos  = Map.map (fun key value -> Unspent value) utxos

    (outpoints |> List.distinct |> List.length > Map.count utxos) ==> (Option.isNone <| UtxoSet.getUtxos outpoints utxos)