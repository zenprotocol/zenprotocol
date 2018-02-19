module Consensus.Transaction

open Consensus.TxSkeleton
open Consensus.Types
open Consensus.Crypto
open MBrace.FsPickler.Combinators
open Newtonsoft.Json

type SerializationMode =
    | Full
    | WithoutWitness

let serialize mode tx =
    match mode with
    | Full -> tx
    | WithoutWitness -> {tx with witnesses=[]}
    |> JsonConvert.SerializeObject
    |> System.Text.Encoding.ASCII.GetBytes

let deserialize bytes =
    try
        bytes
        |> System.Text.Encoding.ASCII.GetString
        |> JsonConvert.DeserializeObject<Transaction>
        |> Some
    with | _ ->
        None

let hash =
    serialize WithoutWitness >> Hash.compute

let witnessHash =
    //TODO: only serialize witness
    serialize Full >> Hash.compute

let addWitnesses witnesses tx =
    { tx with witnesses = witnesses @ tx.witnesses }

let sign keyPairs tx =
    let txHash = hash tx

    let pkWitnesses =
        List.map (
            fun ((secretKey, publicKey)) -> PKWitness (PublicKey.serialize publicKey, Crypto.sign secretKey txHash)
        ) keyPairs

    //// TODO: Should we also use sighash and not sign entire transaction?
    addWitnesses pkWitnesses tx

let fromTxSkeleton tx =
    {
        inputs = List.map (function
            | TxSkeleton.Input.PointedOutput (outpoint, _) -> Outpoint outpoint
            | TxSkeleton.Input.Mint spend -> Mint spend) tx.pInputs
        outputs = tx.outputs
        witnesses = []
        contract = None
    }

// Temporary stuff until we will have blocks
let rootPKHash = Hash.compute [| 3uy; 235uy; 227uy; 69uy; 160uy; 193uy; 130uy; 94uy; 110uy; 75uy; 201uy;
                                 131uy; 186uy; 13uy; 173uy; 220uy; 244uy; 192uy; 5uy; 17uy; 204uy; 211uy;
                                 80uy; 60uy; 34uy; 149uy; 101uy; 37uy; 19uy; 1uy; 22uy; 53uy; 147uy|]

// Temporary transaction until we will have blocks and test genesis block
let rootTx=
    {
        inputs=[];
        outputs=[{lock = PK rootPKHash; spend= {asset = Constants.Zen;amount=100000000UL}}];
        witnesses=[]
        contract=None
    }

let rootTxHash = hash rootTx
