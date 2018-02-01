module Consensus.TransactionValidation

open TxSkeleton
open Consensus.Types
open Consensus.UtxoSet
open Consensus.Crypto

let private (>=>) f1 f2 x = Result.bind f2 (f1 x)

type ValidationError =
    | Orphan
    | DoubleSpend
    | ContractNotActive
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

let private checkAmounts (txSkeleton:TxSkeleton,tx) = 
    let inputs, outputs = foldSpends (txSkeleton.pInputs |> List.map snd), foldSpends txSkeleton.outputs

    if not <| checkSpends outputs then
        GeneralError "outputs overflow"
    else if not <| checkSpends inputs then
        GeneralError "inputs overflow"
    else if outputs <> inputs then
        GeneralError "invalid amounts"
    else
        Ok tx

let getContractWallet tx inputs cHash =     
    List.zip tx.inputs inputs
    |> List.filter (fun (_,output) -> output.lock = Contract cHash)                                        

let private checkWitnesses acs (Hash.Hash txHash, tx, inputs) =       
    let checkPKWitness inputTx pInputs serializedPublicKey signature =
        match pInputs with
        | [] -> GeneralError "missing PK witness input" 
        | (_, {lock=PK pkHash}) :: tail -> 
            match PublicKey.deserialize serializedPublicKey with
            | Some publicKey ->
                if PublicKey.hashSerialized serializedPublicKey = pkHash then
                    match verify publicKey signature txHash with
                    | Valid -> Ok (inputTx, tail)
                    | _ -> GeneralError "invalid PK witness signature"
                else GeneralError "PK witness mismatch"
            | _ -> GeneralError "invalid PK witness"
        | _ -> GeneralError "unexpected PK witness lock type"

    let checkContractWitness inputTx acs cw pInputs =
        let checkIssuedAndDestroyed cw txSkeleton =
            let endInputs = cw.beginInputs + cw.inputsLength - 1u |> int
            let endOutputs = cw.beginOutputs + cw.outputsLength - 1u |> int
            
            if  endInputs >= List.length txSkeleton.pInputs ||
                endOutputs >= List.length txSkeleton.outputs
            then 
                GeneralError "invalid contract witness indices"
            else if 
                txSkeleton.pInputs.[int cw.beginInputs .. endInputs]
                |> List.exists (fun (outpoint, {spend=spend}) ->
                    TxSkeleton.isSkeletonOutpoint outpoint && spend.asset <> cw.cHash)
            then 
                GeneralError "illegal creation of tokens"
            else if
                txSkeleton.outputs.[int cw.beginOutputs .. endOutputs]
                |> List.exists (function
                    | {lock=Destroy; spend=spend} when spend.asset <> cw.cHash -> true
                    | _ -> false)
            then 
                GeneralError "illegal destruction of tokens"
            else 
                Ok txSkeleton
                
        let rec popContractsLocksOf cHash pInputs =
            match pInputs with
            | [] -> [] 
            | (input, output) :: tail ->
                match output.lock with 
                | Contract cHash' when cHash' = cHash ->
                    popContractsLocksOf cHash' tail
                | _ -> (input, output) :: tail                

        match ActiveContractSet.tryFind cw.cHash acs with
        | Some contract ->
            let contractWallet = getContractWallet tx inputs cw.cHash
            
            Contract.run contract cw.command contractWallet inputTx
            |> Result.mapError General
            |> Result.bind (checkIssuedAndDestroyed cw)
            |> Result.bind (fun outputTx ->
                if List.length outputTx.pInputs - List.length inputTx.pInputs = int cw.inputsLength && 
                   List.length outputTx.outputs - List.length inputTx.outputs = int cw.outputsLength then
                    
                    Ok (outputTx, popContractsLocksOf cw.cHash pInputs)
                else GeneralError "input/output length mismatch")
        | None -> Error ContractNotActive

    let witnessesFolder state witness =
        state 
        |> Result.bind (fun (tx', pInputs) ->
            match witness with
            | PKWitness (serializedPublicKey, signature) ->
                checkPKWitness tx' pInputs serializedPublicKey signature
            | ContractWitness cw -> 
                checkContractWitness tx' acs cw pInputs
        )

    let applyMaskIfContract pTx =
        match List.tryPick
            (function
            | ContractWitness cw -> Some cw
            | _ -> None) tx.witnesses with
        | Some cw -> 
            TxSkeleton.applyMask pTx cw
        | _ -> 
            Ok pTx

    TxSkeleton.fromTransaction tx inputs 
    |> Result.bind applyMaskIfContract
    |> Result.mapError General
    |> Result.bind (fun tx' -> 
        List.fold witnessesFolder (Ok (tx', tx'.pInputs)) tx.witnesses)
    |> Result.bind (
        fun (tx', pInputs) -> 
            if not <| List.isEmpty pInputs then
                GeneralError "missing witness(s)"
            else if not <| TxSkeleton.isSkeletonOf tx' tx inputs then
                GeneralError "contract validation failed"
            else 
                Ok (tx',tx)
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

let private checkOrphan getUTXO set txHash tx =
    match getUtxos getUTXO tx.inputs set with
    | Some utxos -> 
        Ok (txHash, tx, utxos)
    | None -> 
        match UtxoSet.isSomeSpent getUTXO tx.inputs set with
        | true -> DoubleSpend
        | false -> Orphan
        |> Error

let validateBasic = 
    checkInputsNotEmpty
    >=> checkOutputsNotEmpty
    >=> checkOutputsOverflow
    >=> checkDuplicateInputs
    >=> checkInputsStructure

let validateInputs getUTXO acs set txHash =
    checkOrphan getUTXO set txHash
    >=> checkWitnesses acs
    >=> checkAmounts