module Consensus.Tests.TransactionTests

open Consensus
open Consensus.Types
open FsCheck.Xunit
open FsCheck

[<Property>]
let ``Transaction serialized and deserialzed equals origin transaction``(tx:Transaction) =
    Transaction.deserialize (Transaction.serialize tx) = tx 
    
[<Property>]
let ``Different transaction doesn't serialize to same bytes``(tx1:Transaction) (tx2:Transaction) =
    (tx1 <> tx2) ==> (Transaction.serialize tx1 <> Transaction.serialize tx2)