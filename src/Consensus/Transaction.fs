module Consensus.Transaction

open Consensus.Types
open Consensus.UtxoSet
open Consensus.Crypto

open MBrace.FsPickler.Combinators

let pickler = Pickler.auto<Transaction>

type SerializationMode =
    | Full
    | WithoutWitness

let serialize mode tx =
    let tx =
        match mode with
        | Full -> tx
        | WithoutWitness -> {tx with witnesses=[]}

    Binary.pickle pickler tx

let deserialize tx =
    try
        Some (Binary.unpickle pickler tx) with
    | _ -> None

let hash =
    serialize WithoutWitness >> Hash.compute

let sign tx keyPairs =
    let txHash = hash tx

    //// TODO: Should we also use sighash and not sign entire transaction?
    { tx with
        witnesses =
            List.map (
                fun ((secretKey, publicKey)) -> PKWitness (PublicKey.serialize publicKey, Crypto.sign secretKey txHash)
            ) keyPairs }

let addWitness witness tx =
    { tx with witnesses = List.append tx.witnesses [ witness ] }

let fromTxSkeleton txSkeleton =
    {
        inputs = []
        outputs = []
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
        outputs=[{lock = PK rootPKHash; spend= {asset = Hash.zero;amount=100000000UL}}];
        witnesses=[]
        contract=None
    }

let rootTxHash = hash rootTx
