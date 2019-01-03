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

let toExtended tx =
    let raw = Transaction.serialize Full tx

    {
        tx=tx
        txHash = hash tx
        witnessHash = Hash.compute raw
        raw=raw
    }

let pushWitnesses witnesses tx =
    { tx with witnesses = witnesses @ tx.witnesses }

let sign keyPairs initialSigHash tx =
    let txHash = hash tx

    let sign (secretKey, publicKey) sigHash tx =
        let msg =
            match sigHash with
            | TxHash -> txHash
            | FollowingWitnesses ->
                let witnessesHash = Serialization.Witnesses.hash tx.witnesses
                Hash.joinHashes [ txHash; witnessesHash ]
            | _ -> failwith "unknown sigHash"

        let witness = PKWitness (sigHash, publicKey, Crypto.sign secretKey msg)

        {tx with witnesses = witness :: tx.witnesses}

    // We sign from last to first
    // Only the last get signed with the initial sigHash, the rest get signed with TxHash
    List.foldBack (fun keyPair (tx,sigHash) ->
        let tx = sign keyPair sigHash tx
        tx,TxHash) keyPairs (tx,initialSigHash)
    |> fst

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

let fromRaw (raw:RawTransaction) =
    {
        version = raw.version
        inputs = raw.inputs
        outputs = raw.outputs
        witnesses = List.choose (fun w ->
            match w with
            | Witness w -> Some w
            | _ -> None) raw.witnesses
        contract = raw.contract
    }

let isOutputSpendable output =
    match output.lock with
    | PK _
    | Vote _
    | Coinbase _
    | Contract _ -> true
    | Fee
    | Destroy
    | ActivationSacrifice
    | ExtensionSacrifice _ -> false
    | HighVLock (identifier,_) -> identifier % 2ul = 0ul