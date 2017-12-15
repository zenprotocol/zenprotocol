module Consensus.Transaction

open Consensus.Types
open Consensus.UtxoSet
open Consensus.Crypto

open MBrace.FsPickler.Combinators

let pickler = Pickler.auto<Transaction>

type SerializationMode = 
    | Full
    | WithoutWitness

type ValidationError =
    | Orphan of Transaction
    | DoubleSpend of Transaction
    | General of string

let private addSpend s m =
    let (+) a b =
        try 
            Some (Operators.Checked.(+) a b) with 
        | :? System.OverflowException -> None
    let value = 
        match Map.tryFind s.asset m with
            | Some (Some v) -> v
            | _ -> 0UL
    Map.add s.asset (value + s.amount) m

let private foldSpends = 
    List.map (fun o -> o.spend) 
    >> List.fold (fun map s -> addSpend s map) Map.empty

let private checkSpends m = 
    Map.exists (fun _ v -> Option.isNone v) m |> not

let private checkAmounts (txHash, tx, inputs) =
    let (==) a b = foldSpends a = foldSpends b
    match tx.outputs == inputs with
        | true -> Ok (txHash, tx, inputs)
        | false -> General "invalid amounts" |> Error

let checkWitnesses (Hash.Hash txHash, tx, inputs) =
    let verify publicKey signature txHash = 
        match Crypto.verify publicKey signature txHash with
            | VerifyResult.Valid -> 
                true
            | VerifyResult.Invalid -> 
                false

    let isValid = 
        match List.length tx.witnesses = List.length inputs with
            | false ->
                false
            | true ->
                List.zip tx.witnesses inputs 
                |> List.fold (
                    fun state (PKWitness (serializedPublicKey, signature), {lock=PK pkHash}) -> 
                        match state with 
                            | false -> 
                                false
                            | true -> 
                                match PublicKey.deserialize serializedPublicKey with
                                | None -> 
                                    false
                                | Some publicKey ->
                                    match PublicKey.hashSerialized serializedPublicKey = pkHash with
                                        | false -> 
                                            false
                                        | true -> 
                                            verify publicKey signature txHash
                ) true
    
    match isValid with 
        | true ->
            Ok tx
        | false ->
            General "invalid witness" |> Error

let private checkInputsNotEmpty tx = 
    match List.isEmpty tx.inputs with
        | true -> General "inputs empty" |> Error
        | false -> Ok tx

let private checkOutputsNotEmpty tx =
    match List.isEmpty tx.outputs with
        | true -> General "outputs empty" |> Error
        | false -> 
            match List.exists (fun output -> output.spend.amount = 0UL) tx.outputs with
                | true -> General "outputs invalid" |> Error
                | false -> Ok tx

let private checkOutputsOverflow tx =
    match tx.outputs 
          |> foldSpends
          |> checkSpends with
        | true -> Ok tx
        | false -> General "outputs overflow" |> Error

let private checkDuplicateInputs tx =
    let (==) a b = List.length a = List.length b
    match List.distinct tx.inputs == tx.inputs with
        | true -> Ok tx
        | false -> General "inputs duplicated" |> Error

let private checkInputsStructure tx =
    match tx.inputs |> List.exists (fun i -> not (Hash.isValid i.txHash)) with
        | true -> General "inputs structurally invalid" |> Error
        | false -> Ok tx

let private checkOrphan set txHash tx =
    match getUtxos tx.inputs set with
    | Some utxos -> 
        Ok (txHash, tx, utxos)
    | None -> 
        Error <| 
            match UtxoSet.isSomeSpent tx.inputs set with
                | true -> DoubleSpend tx
                | false -> Orphan tx

let validateBasic = 
    let (>=>) f1 f2 x = Result.bind f2 (f1 x)

    checkInputsNotEmpty
    >=> checkOutputsNotEmpty
    >=> checkOutputsOverflow
    >=> checkDuplicateInputs
    >=> checkInputsStructure

let validateInputs set txHash =
    let (>=>) f1 f2 x = Result.bind f2 (f1 x)

    checkOrphan set txHash
    >=> checkAmounts
    >=> checkWitnesses

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