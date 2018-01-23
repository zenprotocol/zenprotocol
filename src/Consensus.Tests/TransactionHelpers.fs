module Consensus.Tests.TransactionHelpers

open Consensus
open Consensus.Types
open TransactionValidation

let getSignedTx tx keys =
    let signedTx = Transaction.sign keys tx
    let txHash = Transaction.hash signedTx
    signedTx, txHash

let private inputsValidation acs utxos signedTx txHash keys =
    let tryGetUTXO _ = None
    
    validateInputs tryGetUTXO acs utxos txHash signedTx

let inputsValidationMsg msg acs utxos tx keys =
    let signedTx, txHash = getSignedTx tx keys
    inputsValidation acs utxos signedTx txHash keys, 
    (Error (General msg) : Result<Transaction, ValidationError>)

let inputsValidationOk acs utxos tx keys =
    let signedTx, txHash = getSignedTx tx keys
    inputsValidation acs utxos signedTx txHash keys, 
    (Ok signedTx : Result<Transaction, ValidationError>)

let basicValidationMsg msg tx =
    validateBasic tx, 
    (Error (General msg): Result<Transaction, ValidationError>)

let basicValidationOk tx =
    validateBasic tx, 
    (Ok tx : Result<Transaction, ValidationError>)

let inputsValidationOrphan acs utxos tx keys =
    let signedTx, txHash = getSignedTx tx keys
    inputsValidation acs utxos signedTx txHash keys, 
    (Error Orphan : Result<Transaction, ValidationError>)
