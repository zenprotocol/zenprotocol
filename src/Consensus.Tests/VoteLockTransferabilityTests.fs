module Consensus.Tests.VoteLockTransferabilityTests

open Consensus
open Crypto
open UtxoSet
open Types
open Hash
open NUnit.Framework
open TransactionNunitHelpers
open TransactionHelpers
open FsUnit

let outpoint = getInput 1uy 0ul
let privateKey, publicKey = KeyPair.create()

let makeTx version isVoteInput isVoteOutput isSameAddress =
    let inputAddr, outputAddr =
        PublicKey.hash publicKey,
        match isSameAddress with
        | true -> PublicKey.hash publicKey
        | false -> Hash (Array.create 32 0uy)

    let getOutput isVote addr =
        let lock =
            match isVote with
            | true -> Vote ({ allocation = None; payout = None }, 0u, addr)
            | false -> PK addr
        { lock = lock; spend = { asset = Asset.Zen; amount = 1UL } }

    let utxo = getOutput isVoteInput inputAddr        
    let output = getOutput isVoteOutput outputAddr
    
    {
        version = version
        inputs = [ Outpoint outpoint ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    |> Transaction.sign [(privateKey, publicKey)] TxHash,
    Map.ofSeq [ outpoint, Unspent utxo ] 

let validateTx (tx, utxos) =
    let getUTXO _ = UtxoSet.NoOutput

    TransactionValidation.validateInContext Chain.localParameters getUTXO contractPath 1ul 0UL ActiveContractSet.empty Map.empty utxos getContractState ContractStates.asDatabase (Transaction.toExtended tx)
    |> function
    | Ok _ -> true
    | Error msg -> 
        printfn "%A" msg
        false

let SameAddresses = true
let DifferentAddresses = false
let Vote = true
let PK = false
let allow : bool -> unit = should equal true
let disallow : bool -> unit = should equal false

[<Test>]
let ``Voting should be allowed``() =
    makeTx Version1 PK Vote SameAddresses
    |> validateTx
    |> allow
    
    makeTx Version1 Vote Vote SameAddresses
    |> validateTx
    |> allow

[<Test>]
let ``Vote outputs should not be transferable``() =
    makeTx Version1 PK Vote DifferentAddresses
    |> validateTx
    |> disallow

    makeTx Version0 PK Vote DifferentAddresses
    |> validateTx
    |> allow
    
    makeTx Version1 Vote Vote DifferentAddresses
    |> validateTx
    |> disallow

    makeTx Version0 Vote Vote DifferentAddresses
    |> validateTx
    |> allow

[<Test>]
let ``Unvoting should be allowed``() =
    makeTx Version1 Vote PK DifferentAddresses
    |> validateTx
    |> allow

    makeTx Version1 Vote PK SameAddresses
    |> validateTx
    |> allow