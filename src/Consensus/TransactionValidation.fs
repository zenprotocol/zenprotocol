module Consensus.TransactionValidation

open Consensus.Types
open Consensus.UtxoSet
open Consensus.Crypto

let private (>=>) f1 f2 x = Result.bind f2 (f1 x)

type ValidationError =
    | Orphan of Transaction
    | DoubleSpend of Transaction
    | General of string

//type ACS = Hash.Hash -> Contract.T option

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
    //todo; contract can create it's own tokens
    let (==) a b = foldSpends a = foldSpends b
    match tx.outputs == inputs with
    | true -> Ok (txHash, tx, inputs)
    | false -> General "invalid amounts" |> Error

let private checkWitness acs txHash =
    function
    | (PKWitness (serializedPublicKey, signature), {lock=PK pkHash}) ->
        match PublicKey.deserialize serializedPublicKey with
        | Some publicKey ->
            if PublicKey.hashSerialized serializedPublicKey = pkHash then
                match verify publicKey signature txHash with
                | Valid -> true
                | _ -> false
            else false
        | _ -> false
    | (ContractWitness (cHash,_,_,_,_), {lock=Contract (pkHash, _)}) ->
        match SparseMerkleTree.tryFind cHash acs with
        | Some contract ->
            //TODO: get original txSkeleton from mask
            match Contract.run contract TxSkeleton.empty with 
            | Ok txSkeleton ->
                true //TODO: compare them
            | _ -> false
        | _ -> false
    | _ -> false

let private checkWitnesses acs (Hash.Hash txHash, tx, inputs: Output list) =
    if List.length tx.witnesses = List.length inputs then
        if (List.zip tx.witnesses inputs 
            |> List.exists (checkWitness acs txHash >> not)) then
            General "invalid witness(es)" |> Error
        else
            Ok tx
    else 
        General "witness count mismatch" |> Error
    
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
    checkInputsNotEmpty
    >=> checkOutputsNotEmpty
    >=> checkOutputsOverflow
    >=> checkDuplicateInputs
    >=> checkInputsStructure

let validateInputs acs set txHash =
    checkOrphan set txHash
    >=> checkAmounts
    >=> checkWitnesses acs