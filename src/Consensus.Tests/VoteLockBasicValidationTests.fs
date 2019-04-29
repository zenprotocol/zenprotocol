module Consensus.Tests.VoteLockBasicValidationTests

open Consensus
open Crypto
open UtxoSet
open Types
open Hash
open NUnit.Framework
open TransactionNunitHelpers
open TransactionHelpers
open FsUnit
open ValidationError
open FsCheck.NUnit
open FsCheck

let outpoint = getInput 1uy 0ul
let privateKey, publicKey = KeyPair.create()

let makeTx version lock spend =
    {
        version = version
        inputs = [ Outpoint outpoint ]
        witnesses = []
        outputs = [ { lock = lock; spend = spend } ]
        contract = None
    }
    |> Transaction.sign [(privateKey, publicKey)] TxHash

let expectError error tx = 
    match TransactionValidation.validateBasic tx with
    | Error (General error') -> 
        error' |> should equal error 
    | other ->
        failwithf "Expected %A but was %A" error other

let expectOk tx = 
    match TransactionValidation.validateBasic tx with
    | Error error -> 
        failwithf "Expected Ok but was %A" error
    | Ok _ ->
        ()

[<Test>]
let ``Should invalidate a transaction with voting lock with invalid amount``() =
    let spend = { asset = Asset.Zen; amount = 0UL }
    let lock = Vote ({ allocation = None; payout = None }, 1u, Hash (Array.create 32 0uy))

    makeTx Version0 lock spend
    |> expectError "structurally invalid output(s)"
    
    makeTx Version1 lock spend
    |> expectError "structurally invalid output(s)"

[<Test>]
let ``Should invalidate a transaction with voting lock with non Zen asset``() =
    let nonZen = Asset.defaultOf <| ContractId (Version0, Hash (Array.create 32 1uy))
    let spend = { asset = nonZen; amount = 10UL }
    let lock = Vote ({ allocation = None; payout = None }, 1u, Hash (Array.create 32 0uy))

    makeTx Version0 lock spend
    |> expectOk

    makeTx Version1 lock spend
    |> expectError "structurally invalid vote locks(s)"
    
[<Test>]
let ``Should invalidate a transaction with zero payout ``() =
    let recipient = Recipient.PKRecipient <| Hash (Array.create 32 0uy)
    let spend = { asset = Asset.Zen; amount = 10UL }
    let lock = Vote ({ allocation = None; payout = Some <| (recipient, 0UL) }, 1u, Hash (Array.create 32 0uy))

    makeTx Version1 lock spend
    |> expectError "structurally invalid vote locks(s)"

    let lock = Vote ({ allocation = None; payout = Some <| (recipient, 1UL) }, 1u, Hash (Array.create 32 0uy))

    makeTx Version1 lock spend
    |> expectOk

     // With allocation
     
    let lock = Vote ({ allocation = Some 1uy; payout = Some <| (recipient, 0UL) }, 1u, Hash (Array.create 32 0uy))

    makeTx Version1 lock spend
    |> expectError "structurally invalid vote locks(s)"

    let lock = Vote ({ allocation = Some 1uy; payout = Some <| (recipient, 1UL) }, 1u, Hash (Array.create 32 0uy))

    makeTx Version1 lock spend
    |> expectOk


[<Test>]
let ``Should invalidate a transaction with voting lock with empty vote``() =
    let spend = { asset = Asset.Zen; amount = 1UL }
    let lock = Vote ({ allocation = None; payout = None }, 1u, Hash (Array.create 32 0uy))

    makeTx Version0 lock spend
    |> expectOk

    makeTx Version1 lock spend
    |> expectError "structurally invalid vote locks(s)"

[<Property>]
let ``Should invalidate a transaction with voting lock with allocation bigger than 99 percent``(percent: byte) =
    percent > 99uy ==> (
        let spend = { asset = Asset.Zen; amount = 1UL }
        let lock = Vote ({ allocation = Some percent; payout = None }, 1u, Hash (Array.create 32 0uy))

        makeTx Version0 lock spend
        |> expectOk

        makeTx Version1 lock spend
        |> TransactionValidation.validateBasic
        |> (=) (Error (General "structurally invalid vote locks(s)"))
    )
    
[<Test>]
let ``Should invalidate a transaction with voting with partial amount``() =
    let spend = { asset = Asset.Zen; amount = 10UL }
    let lock = Vote ({ allocation = None; payout = None }, 0u, PublicKey.hash publicKey)

    makeTx Version0 lock spend
    |> expectOk

    let tx = makeTx Version1 lock spend

    let getUTXO _ = UtxoSet.NoOutput
    let utxos = Map.ofSeq [ outpoint, Unspent { tx.outputs.[0] with spend = { asset = Asset.Zen; amount = 1UL } } ] 

    let expected = "invalid amounts"

    TransactionValidation.validateInContext Chain.localParameters getUTXO contractPath 0ul 0UL ActiveContractSet.empty Map.empty utxos getContractState ContractStates.asDatabase (Transaction.toExtended tx)
    |> function 
    | Error (General was) when was = expected -> ()
    | other -> failwithf "Expected %A but was %A" expected other

[<Test>]
let ``Should invalidate a transaction with voting with non Zen partial amount``() =
    let nonZen = Asset.defaultOf <| ContractId (Version0, Hash (Array.create 32 1uy))
    let spend = { asset = nonZen; amount = 10UL }
    let lock = Vote ({ allocation = None; payout = None }, 0u, PublicKey.hash publicKey)

    makeTx Version0 lock spend
    |> expectOk

    let tx = makeTx Version1 lock spend

    let getUTXO _ = UtxoSet.NoOutput
    let utxos = Map.ofSeq [ outpoint, Unspent { tx.outputs.[0] with spend = { asset = Asset.Zen; amount = 1UL } } ] 

    let expected = "invalid amounts"

    TransactionValidation.validateInContext Chain.localParameters getUTXO contractPath 0ul 0UL ActiveContractSet.empty Map.empty utxos getContractState ContractStates.asDatabase (Transaction.toExtended tx)
    |> function 
    | Error (General was) when was = expected -> ()
    | other -> failwithf "Expected %A but was %A" expected other
