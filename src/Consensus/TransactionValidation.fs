module Consensus.TransactionValidation

open TxSkeleton
open Consensus.Types
open Consensus.UtxoSet
open Consensus.Crypto

let private (>=>) f1 f2 x = Result.bind f2 (f1 x)

type ValidationError =
    | Orphan
    | DoubleSpend
    | General of string

let private addSpend s m =
    let (+) a b =
        try 
            Some (Operators.Checked.(+) a b) with 
        | :? System.OverflowException -> None
    match Map.tryFind s.asset m with
    | Some (Some v) -> Map.add s.asset (v + s.amount) m
    | Some None -> m
    | None -> Map.add s.asset (0UL + s.amount) m

let private foldSpends = 
    List.map (fun o -> o.spend) 
    >> List.fold (fun map s -> addSpend s map) Map.empty

let private checkSpends m = 
    Map.exists (fun _ v -> Option.isNone v) m |> not

let private checkAmounts (tx, inputs) =
    let outputs', inputs' = foldSpends tx.outputs, foldSpends inputs

    if not <| checkSpends outputs' then
        "outputs overflow" |> General |> Error
    else if not <| checkSpends inputs' then
        "inputs overflow" |> General |> Error
    else if outputs' <> inputs' then
        "invalid amounts" |> General |> Error
    else
        Ok tx

let private checkWitnesses acs set (Hash.Hash txHash, tx, inputs) =
    let rec popContractsLocksOf cHash tx (pInputs: (Outpoint * Output) list) =
        match pInputs with
        | [] -> [] 
        | (input, output) :: tail ->
            match output.lock with 
            | Contract cHash' when cHash' = cHash ->
                popContractsLocksOf cHash' tx tail
            | _ ->
                (input, output) :: tail

    let checkIssuedAndDestroyed txSkeleton (witness:ContractWitness) =
        txSkeleton.pInputs.[witness.beginInputs..witness.beginInputs + witness.inputsLength]
        |> List.forall (
            fun (input, output) ->
                input.txHash = Hash.zero && 
                input.index = 0ul &&
                output.spend.asset = witness.cHash &&
                output.lock = Contract witness.cHash) &&
        txSkeleton.outputs.[witness.beginOutputs..witness.beginOutputs + witness.outputsLength]
        |> List.forall (
            fun output ->
                output.lock = Destroy && output.spend.asset = witness.cHash)

    let checkPKWitness tx pInputs serializedPublicKey signature =
        let toError err =
            sprintf "PK witness: %s" err |> General |> Error
        match pInputs with
        | [] -> toError "missing input" 
        | (_, {lock=PK pkHash}) :: tail -> 
            match PublicKey.deserialize serializedPublicKey with
            | Some publicKey ->
                if PublicKey.hashSerialized serializedPublicKey = pkHash then
                    match verify publicKey signature txHash with
                    | Valid ->
                        Ok (tx, tail)
                    | _ -> toError "invalid signature"
                else toError "mismatch"
            | _ -> toError "invalid"
        | _ -> toError "unexpected lock type"

    let checkContractWitness tx acs set witness pInputs =
        match ActiveContractSet.tryFind witness.cHash acs with
        | Some contract ->
            match Contract.run contract set tx with 
            | Ok tx' ->
                if checkIssuedAndDestroyed tx' witness then
                    let pInputs = popContractsLocksOf witness.cHash tx' pInputs //witness.inputsLength //1!!!
                    if List.length tx'.pInputs - List.length tx.pInputs = witness.inputsLength &&
                       List.length tx'.outputs - List.length tx.outputs = witness.outputsLength then
                        Ok (tx', pInputs)
                    else
                        "input/output length mismatch" |> General |> Error
                else
                    "illegal creation/destruction of tokens" |> General |> Error

            | Error err -> err |> General |> Error
        | None -> "contract is not active" |> General |> Error

    //TODO: check the contract witness is single and last
    let firstContractWitness = 
        tx.witnesses
        |> List.choose (
            function
            | ContractWitness cw -> Some cw
            | _ -> None)
        |> List.tryHead 
       
    let pInputs = List.zip tx.inputs inputs

    let tx' = 
        match firstContractWitness with
        | Some cw -> TxSkeleton.applyMask tx cw inputs
        | None -> TxSkeleton.fromTransaction tx inputs
            
    let witnessesFolder state witness =
        state 
        |> Result.bind (fun (tx', pInputs) ->
            match witness with
            | PKWitness (serializedPublicKey, signature) ->
                checkPKWitness tx' pInputs serializedPublicKey signature
            | ContractWitness cw ->
                checkContractWitness tx' acs set cw pInputs)

    tx.witnesses
    |> List.fold witnessesFolder (Ok (tx', pInputs))
    |> Result.bind (
        fun (tx'', pInputs) ->
            if not <| List.isEmpty pInputs then
                "missing witness(s)" |> General |> Error
            else if not <| TxSkeleton.isSkeletonOf tx'' tx then
                "contract validation failed" |> General |> Error
            else 
                Ok (tx, inputs) //, tx'') 
    )

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
            | true -> DoubleSpend
            | false -> Orphan

let validateBasic = 
    checkInputsNotEmpty
    >=> checkOutputsNotEmpty
    >=> checkOutputsOverflow
    >=> checkDuplicateInputs
    >=> checkInputsStructure

let validateInputs acs set txHash =
    checkOrphan set txHash
    >=> checkWitnesses acs set
    >=> checkAmounts