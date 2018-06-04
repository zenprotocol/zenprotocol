module Consensus.Transaction

open Consensus.Serialization
open TxSkeleton
open Types
open Crypto
open Serialization

let hash =
    Transaction.serialize WithoutWitness >> Hash.compute

let toHex =
    Transaction.serialize Full >> FsBech32.Base16.encode

let fromHex hex =
    FsBech32.Base16.decode hex
    |> Option.bind (Transaction.deserialize Full)

let witnessHash =
    //TODO: only serialize witness
    Transaction.serialize Full >> Hash.compute

let addWitnesses witnesses tx =
    { tx with witnesses = witnesses @ tx.witnesses }

let sign keyPairs tx =
    let txHash = hash tx

    let pkWitnesses =
        List.map (
            fun ((secretKey, publicKey)) -> PKWitness (publicKey, Crypto.sign secretKey txHash)
        ) keyPairs

    //// TODO: Should we also use sighash and not sign entire transaction?
    addWitnesses pkWitnesses tx

let fromTxSkeleton tx =
    {
        version = Version0
        inputs = List.map (function
            | TxSkeleton.Input.PointedOutput (outpoint, _) -> Outpoint outpoint
            | TxSkeleton.Input.Mint spend -> Mint spend) tx.pInputs
        outputs = tx.outputs
        witnesses = []
        contract = None
    }

let isOutputSpendable output =
    match output.lock with
    | PK _
    | Coinbase _
    | Contract _
    | HighVLock _ -> true
    | Fee
    | Destroy
    | ActivationSacrifice
    | ExtensionSacrifice _ -> false