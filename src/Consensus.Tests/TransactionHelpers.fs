module Consensus.Tests.TransactionHelpers

open Consensus
open Consensus.Types
open TransactionValidation

let getSignedTx tx keys =
    let signedTx = Transaction.sign keys tx
    let txHash = Transaction.hash signedTx
    signedTx, txHash

let private contractPath = "./test"

let private inputsValidation blockNumber acs utxos signedTx txHash =
    let getUTXO _ = UtxoSet.NoOutput

    validateInContext getUTXO contractPath blockNumber acs utxos txHash signedTx |> Result.map fst

let inputsValidationMsg msg blockNumber acs utxos tx keys =
    let signedTx, txHash = getSignedTx tx keys
    inputsValidation blockNumber acs utxos signedTx txHash,
    (Error (General msg) : Result<Transaction, ValidationError>)

let inputsValidationOk blockNumber acs utxos tx keys =
    let signedTx, txHash = getSignedTx tx keys
    inputsValidation blockNumber acs utxos signedTx txHash,
    (Ok signedTx : Result<Transaction, ValidationError>)

let basicValidationMsg msg tx =
    validateBasic tx,
    (Error (General msg): Result<Transaction, ValidationError>)

let basicValidationOk tx =
    validateBasic tx,
    (Ok tx : Result<Transaction, ValidationError>)

let inputsValidationOrphan blockNumber acs utxos tx keys =
    let signedTx, txHash = getSignedTx tx keys
    inputsValidation blockNumber acs utxos signedTx txHash,
    (Error Orphan : Result<Transaction, ValidationError>)
