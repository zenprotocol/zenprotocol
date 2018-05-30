module Consensus.Tests.TransactionHelpers

open Consensus
open Consensus.Types
open Consensus.Chain
open ValidationError
open TransactionValidation


let getSignedTx tx keys =
    let signedTx = Transaction.sign keys tx
    let txHash = Transaction.hash signedTx
    signedTx, txHash

let contractPath =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]

let private inputsValidation blockNumber timestamp acs utxos signedTx txHash =
    let getUTXO _ = UtxoSet.NoOutput

    validateInContext Chain.localParameters getUTXO contractPath blockNumber timestamp acs Map.empty utxos ContractStates.asDatabase txHash signedTx
    |> Result.map (fun (tx, _, _, _) -> tx)

let inputsValidationMsg msg blockNumber timestamp acs utxos tx keys =
    let signedTx, txHash = getSignedTx tx keys
    inputsValidation blockNumber timestamp acs utxos signedTx txHash,
    (Error (General msg) : Result<Transaction, ValidationError>)

let inputsValidationOk blockNumber timestamp acs utxos tx keys =
    let signedTx, txHash = getSignedTx tx keys
    inputsValidation blockNumber timestamp acs utxos signedTx txHash,
    (Ok signedTx : Result<Transaction, ValidationError>)

let basicValidationMsg msg tx =
    validateBasic tx,
    (Error (General msg): Result<Transaction, ValidationError>)

let basicValidationOk tx =
    validateBasic tx,
    (Ok tx : Result<Transaction, ValidationError>)

let inputsValidationOrphan blockNumber timestamp acs utxos tx keys =
    let signedTx, txHash = getSignedTx tx keys
    inputsValidation blockNumber timestamp acs utxos signedTx txHash,
    (Error Orphan : Result<Transaction, ValidationError>)
