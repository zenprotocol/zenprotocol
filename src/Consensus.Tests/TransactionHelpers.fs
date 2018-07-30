module Consensus.Tests.TransactionHelpers

open Consensus
open Consensus.Types
open Consensus.Chain
open ValidationError
open TransactionValidation

let getContractState _ = None

let getSignedTx tx keys =
    let signedTx = Transaction.sign keys TxHash tx    
    signedTx

let contractPath =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]

let inputsValidation blockNumber timestamp acs utxos signedTx =
    let getUTXO _ = UtxoSet.NoOutput

    validateInContext Chain.localParameters getUTXO contractPath blockNumber timestamp acs Map.empty utxos getContractState ContractStates.asDatabase (Transaction.toExtended signedTx)
    |> Result.map (fun (tx, _, _, _) -> tx.tx)

let inputsValidationMsg msg blockNumber timestamp acs utxos tx keys =
    let signedTx = getSignedTx tx keys
    inputsValidation blockNumber timestamp acs utxos signedTx,
    (Error (General msg) : Result<Transaction, ValidationError>)

let inputsValidationOk blockNumber timestamp acs utxos tx keys =
    let signedTx = getSignedTx tx keys
    inputsValidation blockNumber timestamp acs utxos signedTx,
    (Ok signedTx : Result<Transaction, ValidationError>)

let basicValidationMsg msg tx =
    validateBasic tx,
    (Error (General msg): Result<Transaction, ValidationError>)

let basicValidationOk tx =
    validateBasic tx,
    (Ok tx : Result<Transaction, ValidationError>)

let inputsValidationOrphan blockNumber timestamp acs utxos tx keys =
    let signedTx = getSignedTx tx keys
    inputsValidation blockNumber timestamp acs utxos signedTx,
    (Error Orphan : Result<Transaction, ValidationError>)
