module Consensus.Transaction

open Consensus.Types
open Consensus.UtxoSet
open Consensus.Crypto

open MBrace.FsPickler.Combinators

let pickler = Pickler.auto<Transaction>

type SerializationMode = 
    | Full
    | WithoutWitness

type ValitationError =
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

let private checkAmounts (tx, inputs) =
    let (==) a b = foldSpends a = foldSpends b
    match tx.outputs == inputs with
        | true -> Ok tx
        | false -> General "invalid amounts" |> Error

let private checkInputsNotEmpty tx = 
    match List.isEmpty tx.inputs with
        | true -> General "inputs empty" |> Error
        | false -> Ok tx

let private checkOutputsNotEmpty tx =
    match List.isEmpty tx.outputs with
        | true -> General "outputs empty" |> Error
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

let private checkOrphan set tx =
    match getUtxos tx.inputs set with
    | Some utxos -> 
        Ok (tx, utxos)
    | None -> 
        Error <| 
            match UtxoSet.isSomeSpent tx.inputs set with
                | true -> DoubleSpend tx
                | false -> Orphan tx         

let validateBasic = 
    let (>=>) f1 f2 x = Result.bind f2 (f1 x)

    checkInputsNotEmpty  >=> 
    checkOutputsNotEmpty >=> 
    checkOutputsOverflow >=> 
    checkDuplicateInputs >=> 
    checkInputsStructure

let validateInputs set =
    let (>=>) f1 f2 x = Result.bind f2 (f1 x)

    checkOrphan set >=> 
    checkAmounts

let serialize mode tx =
    let tx = 
        match mode with
            | Full -> tx
            | WithoutWitness -> {tx with witnesses=[]}

    Binary.pickle pickler tx

let deserialize =
    // TODO: should return an option
    Binary.unpickle pickler 

let hash =
    serialize WithoutWitness >> Hash.compute
        
let sign tx secretKey =
    let txHash = hash tx

    let witnessInput _ = 
        PKWitness (Crypto.sign secretKey txHash)
         
    // TODO: Should we also use sighash and not sign entire transaction?
    let witnesses = List.foldBack (fun input witnesses -> 
        (witnessInput input) :: witnesses) tx.inputs []         
        
    {tx with witnesses = witnesses}