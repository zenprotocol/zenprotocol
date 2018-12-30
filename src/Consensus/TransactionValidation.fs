module Consensus.TransactionValidation

open Consensus
open Chain
open Types
open UtxoSet
open Crypto
open Zen.Types.Data
open Infrastructure
open Result
open ValidationError
open Logary.Message

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
    List.fold (fun map s -> addSpend s map) Map.empty

let private checkSpends m =
    Map.forall (fun _ v -> Option.isSome v) m

let private getSacrificeBlocks (chainParams : Chain.ChainParameters) code sacrifice =
    let codeLength = String.length code |> uint64
    let activationSacrificePerBlock = chainParams.sacrificePerByteBlock * codeLength
    let numberOfBlocks = sacrifice / activationSacrificePerBlock |> uint32

    if numberOfBlocks = 0ul then
        GeneralError "Contract must be activated for at least one block"
    else
        Ok numberOfBlocks

let private activateContract (chainParams : Chain.ChainParameters) contractPath blockNumber acs contractCache (tx : Types.Transaction) =
    let getActivationSacrifice tx = result {
        let activationSacrifices = List.filter(fun output -> output.lock = ActivationSacrifice) tx.outputs

        if List.isEmpty activationSacrifices then
            return! GeneralError "Contract activation must include activation sacrifice"

        let allUsingZen =
            List.forall (fun output -> output.spend.asset = Asset.Zen) activationSacrifices

        if not allUsingZen then
            return! GeneralError "Sacrifice must be paid in Zen"

        return List.sumBy (fun output -> output.spend.amount) activationSacrifices
    }

    result {
        match tx.contract with
        | Some contract ->
            match contract with
            | V0 contract ->
                match ZFStar.totalQueries contract.hints with
                | Error error ->
                    yield! GeneralError error
                | Ok value when value <> contract.queries ->
                    yield! GeneralError "Total queries mismatch"
                | _ -> ()

                let contractId = Contract.makeContractId Version0 contract.code

                let! activationSacrifices = getActivationSacrifice tx

                let! numberOfBlocks = getSacrificeBlocks chainParams contract.code activationSacrifices
                let expiry = blockNumber + numberOfBlocks - 1ul

                match ActiveContractSet.tryFind contractId acs with
                | Some contract ->
                    let contract = {contract with expiry = contract.expiry + numberOfBlocks}
                    return ActiveContractSet.add contractId contract acs, contractCache
                | None ->
                    let! contract, contractCache = result {
                        match ContractCache.tryFind contractId contractCache with
                        | Some (mainFn, costFn) ->
                            return (
                                {
                                    contractId = contractId
                                    mainFn = mainFn
                                    costFn = costFn
                                    expiry = expiry
                                    code = contract.code
                                } : Contract.T), contractCache
                        | None ->
                            let compile contract =
                                Contract.compile contractPath contract
                                |> Result.bind (fun _ -> Contract.load contractPath expiry contract.code contractId)
                                |> Result.mapError (fun error ->
                                    eventX "Contract activation failed: {error}"
                                    >> setField "error" (sprintf "%A" error)
                                    |> Log.info
                                    BadContract)

                            let! contract = Measure.measure (sprintf "compiling contract %A" contractId) (lazy (compile contract))
                            let contractCache = ContractCache.add contract contractCache

                            return contract, contractCache
                    }
                    return ActiveContractSet.add contract.contractId contract acs, contractCache
            | HighV _ ->
                //return acs, contractCache
                yield! GeneralError "High version contracts not supported"
        | None ->
            return acs, contractCache
    }

let private extendContracts chainParams acs tx = result {
    let extensionSacrifices = List.filter(function | { lock = ExtensionSacrifice _ } -> true | _ -> false) tx.outputs
    if List.exists (fun output -> output.spend.asset <> Asset.Zen) extensionSacrifices then
        return! GeneralError "Sacrifice must be paid in Zen"

    return!
        extensionSacrifices
        |> List.choose (function | { lock = ExtensionSacrifice cHash; spend = { amount = amount } } -> Some (cHash, amount)
                                 | _ -> None)
        |> List.fold (fun acs (contractId, amount) -> result {
            let! acs = acs
            let! contract = match ActiveContractSet.tryFind contractId acs with
                            | Some contract -> Ok contract
                            | _ -> GeneralError "Contract(s) must be active"
            let! blocks = getSacrificeBlocks chainParams contract.code amount
            let contract = {contract with expiry = contract.expiry + blocks - 1ul}
            return ActiveContractSet.add contract.contractId contract acs
        }) (Ok acs)
}

let private checkAmounts (txSkeleton:TxSkeleton.T) =
    let inputs = List.map (function | TxSkeleton.Input.PointedOutput (_, output) -> output.spend | TxSkeleton.Input.Mint spend -> spend) txSkeleton.pInputs
    let inputs, outputs = foldSpends inputs, foldSpends (List.map (fun o -> o.spend) txSkeleton.outputs)

    if not <| checkSpends outputs then
        GeneralError "outputs overflow"
    else if not <| checkSpends inputs then
        GeneralError "inputs overflow"
    else if outputs <> inputs then
        GeneralError "invalid amounts"
    else
        Ok ()

let private checkInputsVersion (txSkeleton:TxSkeleton.T) =
    if List.exists (function
        | TxSkeleton.PointedOutput (_, { lock = HighVLock _ }) -> true
        | _ -> false) txSkeleton.pInputs then
        GeneralError "high version inputs are invalid"
    else
        Ok()

let checkWeight chain tx txSkeleton = 
    Weight.transactionWeight tx txSkeleton
    >>= (fun txWeight ->
        if txWeight <= chain.maxBlockWeight then
            Ok()
        else
            Error "transaction weight exceeds maximum")
    |> Result.mapError General

let private checkVoteLocks (txSkeleton:TxSkeleton.T) =
    let pkHashes =
        txSkeleton.outputs
        |> List.choose (function
            | { spend = { amount = _; asset = asset }; lock = Vote (_, _, pkHash) } ->
                Some pkHash
            | _ ->
                None)

    let isMissing pkHash =
        txSkeleton.pInputs
        |> List.choose (function
            | TxSkeleton.Input.PointedOutput (_, output) -> Some output.lock
            | _ -> None)
        |> List.choose (function
            | Coinbase (_, pkHash)
            | PK pkHash
            | Vote (_, _, pkHash) -> Some pkHash
            | _ -> None)
        |> List.contains pkHash
        |> not

    if List.exists isMissing pkHashes then
        GeneralError "vote outputs should not be transferable"
    else
        Ok ()
        
let private checkOutputsOverflow tx =
    if tx.outputs
        |> List.map (fun o -> o.spend)
        |> foldSpends
        |> checkSpends then Ok tx
    else GeneralError "outputs overflow"

let private checkDuplicateInputs tx =
    let (==) a b = List.length a = List.length b
    if List.distinct tx.inputs == tx.inputs then Ok tx
    else GeneralError "inputs duplicated"

let private checkMintsOnly tx =
    if List.forall (function | Mint _ -> true | _ -> false) tx.inputs then
        GeneralError "inputs consist of mints only"
    else
        Ok tx

let private checkVersion (tx:Transaction) =
    if tx.version > Version1 then
        GeneralError "unsupported transaction version"
    else
        Ok tx

let private checkStructure =
    let isInvalidSpend = fun { asset = (Asset (ContractId (version,cHash), subType)); amount = amount } ->
        cHash = Hash.zero && (subType <> Hash.zero || version <> Version0) ||
            amount = 0UL //Relieve for non spendables?
    let isNull = function | null -> true | _ -> false
    let isEmptyArr arr = isNull arr || Array.length arr = 0
    let isEmptyString str = isNull str || String.length str = 0

    let checkContractStructure = fun tx ->
        match tx.contract with
        | Some contract ->
            match contract with
            | V0 contract when
                isEmptyString contract.code ||
                isEmptyString contract.hints ||
                contract.rlimit = 0u ||
                contract.queries = 0u -> false
            | HighV (version, bytes) when
                version = Version0 || //reserved for V0
                isEmptyArr bytes -> false
            | _ -> true
        | None -> true
        |> function
        | false ->
            GeneralError "structurally invalid contract data"
        | true -> Ok tx

    let checkInputsStructrue = fun tx ->
        if List.isEmpty tx.inputs || List.exists (function
            | Mint spend -> isInvalidSpend spend
            | _ -> false
        ) tx.inputs then
            GeneralError "structurally invalid input(s)"
        else
            Ok tx

    let checkOutputsStructrue = fun tx ->
        if List.isEmpty tx.outputs || List.exists (fun { lock = lock; spend = spend } ->
            match lock with
            | HighVLock (identifier, bytes) ->
                identifier <= Serialization.Serialization.Lock.LastReservedIdentifier
                || isEmptyArr bytes
            | _ ->
                false
            || isInvalidSpend spend
        ) tx.outputs then
            GeneralError "structurally invalid output(s)"
        else
            Ok tx

    let checkVoteLocks = fun (tx:Transaction) ->
        if tx.version = Version1 && List.exists (function
            | { lock = Vote ({ allocation = None; payout = None }, _, _); spend = _ } ->
                true
            | { lock = Vote ({ allocation = Some x; payout = _ }, _, _); spend = _ } when x > 99uy ->
                true
            | { lock = Vote _; spend = spend } when spend.asset <> Asset.Zen ->
                true
            | _ ->
                false
        ) tx.outputs then
            GeneralError "structurally invalid vote locks(s)"
        else
            Ok tx

    let checkWitnessesStructure = fun tx ->
        if List.isEmpty tx.witnesses || List.exists (function
            | ContractWitness cw ->
                isNull cw.command || 
                cw.cost = 0UL || 
                (int cw.beginInputs > List.length tx.inputs - 1 && cw.inputsLength > 0u) ||
                int cw.endInputs > List.length tx.inputs - 1
            | HighVWitness (identifier, bytes) ->
                identifier <= 2u // last reserved identifier for witness types
                || isEmptyArr bytes
            | _ -> false
        ) tx.witnesses then
            GeneralError "structurally invalid witness(es)"
        else
            Ok tx

    checkContractStructure
    >=> checkInputsStructrue
    >=> checkOutputsStructrue
    >=> checkVoteLocks
    >=> checkWitnessesStructure

let private checkNoCoinbaseLock tx =
    let anyCoinbase =
        List.exists (fun output ->
            match output.lock with
            | Coinbase _ -> true
            | _ -> false) tx.outputs

    if anyCoinbase then
        GeneralError "coinbase lock is not allowed within an ordinary transaction"
    else
        Ok tx

let private checkActivationSacrifice tx =
    let isActivationSacrifise lock =
        match lock with
        | ActivationSacrifice _ -> true
        | _ -> false

    let anySacrifice = List.exists (fun output -> isActivationSacrifise output.lock) tx.outputs

    if anySacrifice = Option.isSome tx.contract then
        Ok tx
    else
        if anySacrifice then
            GeneralError "tx with an activation sacrifice must include a contract"
        else
            GeneralError "tx with a contract must include an activation sacrifice"


let private tryGetUtxos getUTXO utxoSet tx =
    getUtxosResult getUTXO tx.inputs utxoSet
    |> Result.mapError (fun errors ->
        if List.exists
            (fun err -> match err with | Spent _ -> true | _ -> false)
            errors
        then DoubleSpend else Orphan
    )

let validateBasic =
    checkVersion
    >=> checkStructure
    >=> checkNoCoinbaseLock
    >=> checkOutputsOverflow
    >=> checkDuplicateInputs
    >=> checkMintsOnly
    >=> checkActivationSacrifice

let validateCoinbase chain blockNumber =
    let isPayout = CGP.isPayoutBlock chain blockNumber
    let checkOnlyCoinbaseLocks blockNumber tx =
        let allCoinbase =
            tx.outputs
            |> if isPayout then List.tail else id
            |> List.forall (fun output ->
                match output.lock with
                | Coinbase (blockNumber',_) when blockNumber' = blockNumber -> true
                | _ -> false)

        if allCoinbase then
            Ok tx
        else
            GeneralError "within coinbase transaction all outputs must use coinbase lock"

    let checkPayout tx =
        let isZen = function { amount = _; asset = asset } -> asset = Asset.Zen
        match isPayout, List.head tx.outputs with
        | false, _ ->
            Ok tx
        | true, { lock = PK _; spend = spend } when isZen spend ->
            Ok tx
        | true, { lock = Contract _; spend = spend } when isZen spend ->
            Ok tx
        | true, { lock = Coinbase (blockNumber',_); spend = _ } when blockNumber' = blockNumber ->
            Ok tx
            (* could be in a payout block without an actual payout in the following cases:
                1) payout blocks before the hard-fork
                2) the CGP can't satisfy any winning payout results
                3) no payout votes exist *)
        | _ ->
            GeneralError "payout output mismatch"

    let checkNoInput tx =
        match tx.inputs with
        | [] -> Ok tx
        | _ -> GeneralError "coinbase transaction must not have any inputs"

    let checkNoContract tx =
        if Option.isSome tx.contract then
            GeneralError "coinbase transaction cannot activate a contract"
        else
            Ok tx

    let checkNoWitnesses tx =
        match tx.witnesses with
        | [] -> Ok tx
        | _ -> GeneralError "coinbase transaction must not have any witnesses"

    let checkOutputsNotEmpty tx =
        if List.isEmpty tx.outputs then
            GeneralError "outputs empty"
        else if List.exists (fun output -> output.spend.amount = 0UL) tx.outputs then
            GeneralError "outputs invalid"
        else
            Ok tx

    checkOutputsNotEmpty
    >=> checkNoInput
    >=> checkNoWitnesses
    >=> checkOnlyCoinbaseLocks blockNumber
    >=> checkPayout
    >=> checkNoContract
    >=> checkOutputsOverflow

let validateInContext chainParams getUTXO contractPath blockNumber timestamp acs contractCache set getContractState contractState ex = result {
    let! outputs = tryGetUtxos getUTXO set ex.tx
    let txSkel = TxSkeleton.fromTransaction ex.tx outputs

    do! checkInputsVersion txSkel
    do! checkWeight chainParams ex.tx txSkel

    do! checkAmounts txSkel
    
    if ex.tx.version = Version1 then
        do! checkVoteLocks txSkel
        
    let! contractStates = InputValidation.StateMachine.validate chainParams blockNumber timestamp acs getContractState contractState ex.txHash ex.tx txSkel

    let! acs, contractCache = activateContract chainParams contractPath blockNumber acs contractCache ex.tx
    let! acs = extendContracts chainParams acs ex.tx
    return ex, acs, contractCache, contractStates
}
