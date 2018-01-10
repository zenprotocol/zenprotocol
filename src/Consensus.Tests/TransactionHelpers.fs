module Consensus.Tests.TransactionHelpers

open Consensus
open Consensus.Types
open TransactionValidation

let getSignedTx tx keys =
    let signedTx = Transaction.sign tx keys
    let txHash = Transaction.hash signedTx
    signedTx, txHash

let inputsValidation acs utxos tx keys =
    let signedTx, txHash = getSignedTx tx keys
    validateInputs acs utxos txHash signedTx

let inputsValidationMsg msg acs utxos tx keys =
    let signedTx, _ = getSignedTx tx keys
    inputsValidation acs utxos signedTx keys, 
    (Error (General msg) : Result<Transaction, ValidationError>)

let inputsValidationOk acs utxos tx keys =
    let signedTx, _ = getSignedTx tx keys
    inputsValidation acs utxos signedTx keys, 
    (Ok signedTx : Result<Transaction, ValidationError>)

let basicValidationMsg msg tx =
    validateBasic tx, 
    (Error (General msg): Result<Transaction, ValidationError>)

let basicValidationOk tx =
    validateBasic tx, 
    (Ok tx : Result<Transaction, ValidationError>)
