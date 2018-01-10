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

let private GeneralError msg =
    msg |> General |> Error

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
        GeneralError "outputs overflow"
    else if not <| checkSpends inputs' then
        GeneralError "inputs overflow"
    else if outputs' <> inputs' then
        GeneralError "invalid amounts"
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
        match pInputs with
        | [] -> GeneralError "missing PK witness input" 
        | (_, {lock=PK pkHash}) :: tail -> 
            match PublicKey.deserialize serializedPublicKey with
            | Some publicKey ->
                if PublicKey.hashSerialized serializedPublicKey = pkHash then
                    match verify publicKey signature txHash with
                    | Valid ->
                        Ok (tx, tail)
                    | _ -> GeneralError "invalid PK witness signature"
                else GeneralError "PK witness mismatch"
            | _ -> GeneralError "invalid PK witness"
        | _ -> GeneralError "unexpected PK witness lock type"

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
                        GeneralError "input/output length mismatch"
                else
                    GeneralError "illegal creation/destruction of tokens"

            | Error err -> GeneralError err
        | None -> GeneralError "contract is not active"

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
                GeneralError "missing witness(s)"
            else if not <| TxSkeleton.isSkeletonOf tx'' tx then
                GeneralError "contract validation failed"
            else 
                Ok (tx, inputs) //, tx'') 
    )

let private checkInputsNotEmpty tx = 
    if List.isEmpty tx.inputs then GeneralError "inputs empty"
    else Ok tx

let private checkOutputsNotEmpty tx =
    if List.isEmpty tx.outputs then
        GeneralError "outputs empty"
    else if List.exists (fun output -> output.spend.amount = 0UL) tx.outputs then
        GeneralError "outputs invalid"
    else
        Ok tx

let private checkOutputsOverflow tx =
    if tx.outputs 
          |> foldSpends
          |> checkSpends then Ok tx
    else GeneralError "outputs overflow"

let private checkDuplicateInputs tx =
    let (==) a b = List.length a = List.length b
    if List.distinct tx.inputs == tx.inputs then Ok tx
    else GeneralError "inputs duplicated"

let private checkInputsStructure tx =
    if tx.inputs |> List.exists (fun input -> not (Hash.isValid input.txHash)) then
        GeneralError "inputs structurally invalid"
    else
        Ok tx

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