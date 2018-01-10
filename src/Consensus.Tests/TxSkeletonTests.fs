module Consensus.Tests.TxSkeletonTests

open Consensus
open Consensus.TxSkeleton
open Consensus.Types
open Consensus.Hash
open FsCheck
open FsCheck.NUnit

let testHash = Hash (Array.create 32 1uy)

[<Property>]
let ``Transaction with less inputs or outputs should not be a prefix of another`` (tx1:TxSkeleton) (tx2:TxSkeleton) =
    (
        List.length tx1.pInputs > List.length tx2.pInputs || 
        List.length tx1.outputs > List.length tx2.outputs
    ) ==> lazy (
        TxSkeleton.checkPrefix tx1 tx2 = Error "invalid prefix"
    )

[<Property>]
let ``Transaction with additional single output should be a prefix of another`` (tx:TxSkeleton) =
    let output = {
        lock = PK testHash
        spend = {asset = Hash.zero; amount = 1UL } 
    }
    let tx' = { tx with outputs = output :: tx.outputs }
    TxSkeleton.checkPrefix tx tx' = Ok tx'

[<Property>]
let ``Transaction with additional single input should be a prefix of another`` (tx:TxSkeleton) =
    let input = { 
        txHash = testHash
        index = 0ul 
    }
    let output = {
        lock = PK testHash
        spend = {asset = Hash.zero; amount = 1UL } 
    }
    let tx' = { tx with pInputs = (input, output) :: tx.pInputs }
    TxSkeleton.checkPrefix tx tx' = Ok tx'

[<Property>]
let ``Transactions with different leading first input should not be a prefix of one another`` (tx:TxSkeleton) =
    (List.length tx.pInputs > 0) ==> lazy (
        let input, output = tx.pInputs.[0]
        let pInput = { input with index = input.index + 1ul }, output
        let pInputs = tx.pInputs.[1..List.length tx.pInputs - 1]
        let tx' = { tx with pInputs = pInput :: pInputs }

        TxSkeleton.checkPrefix tx tx' = Error "invalid prefix" &&
        TxSkeleton.checkPrefix tx tx' = Error "invalid prefix"
    )