module Consensus.InputValidation.ContractV0

open Consensus
open Types
open ValidationError
open TxSkeleton
open State
open Zen.Types.Main

let private validateCost contract initialTx context sender contractWallet (w:ContractWitness) contractState =
    let cost = Contract.getCost contract initialTx context w.command sender w.messageBody contractWallet contractState

    if uint64 cost <> w.cost then
        GeneralError "execution cost commitment mismatch"
    else
        Ok ()

let private validateState (contract:Contract.T) witness getContractState contractStates =
    let computeCommitment =
        Serialization.Data.serialize
        >> Hash.compute
        
    match witness.stateCommitment, ContractStates.tryGetState getContractState contract.contractId contractStates with
    | NoState, None ->
        Ok None
    | State commitment, Some given when commitment = computeCommitment given ->
        Ok <| Some given
    | NotCommitted, state ->
        Ok state
    | _ -> 
        GeneralError "state commitment mismatch"

let private checkMask w inputTx outputTx =
    if List.length outputTx.pInputs - List.length inputTx.pInputs = int w.inputsLength &&
       List.length outputTx.outputs - List.length inputTx.outputs = int w.outputsLength then
        Ok ()
    else
        GeneralError "input/output length mismatch"

let rec private validateInputs contractId inputs count =
    match inputs, count with
    | inputs, 0ul -> Ok inputs
    | [], _ -> GeneralError "input/output length mismatch"
    | input :: inputs, count ->
        match input with
        | Mint spend ->
            let contractId' = Asset.contractId spend.asset

            if contractId' <> contractId then
                GeneralError "illegal creation of tokens"
            else
                validateInputs contractId inputs (count - 1ul)
        | PointedOutput (_,output) ->
            match output.lock with
            | Types.Contract contractId' when contractId' = contractId ->
                validateInputs contractId inputs (count - 1ul)
            | _ -> GeneralError "cannot unlock input"

let private validateWitness context acs getContractState contractStates txHash finalTx sender inputs (w:ContractWitness) = result {
    match ActiveContractSet.tryFind w.contractId acs with
    | None ->
        return! Error ContractNotActive
    | Some contract ->
        // apply the wintess mask on the transaction
        let! inputTx =
            TxSkeleton.applyMask finalTx w
            |> Result.mapError General

        let contractWallet = Contract.getContractWallet finalTx w
        
        let! contractState = validateState contract w getContractState contractStates
        
        do! validateCost contract inputTx context sender contractWallet w contractState

        // running the contract
        let! outputTx, message, updatedState =
             Contract.run contract inputTx context w.command sender w.messageBody contractWallet contractState
             |> Result.mapError General
             
        let contractStates = 
            match updatedState with
            | stateUpdate.Delete -> 
                ContractStates.delete contract.contractId contractStates
            | stateUpdate.NoChange ->
                contractStates
            | stateUpdate.Update data ->
                ContractStates.update contract.contractId data contractStates

        // check that the output tx is subset of the final tx
        // TODO: once refactoring is completed we can make checkPrefix return unit instead of tx and then use do! instead of let!
        let! _ =
            TxSkeleton.checkPrefix outputTx finalTx
            |> Result.mapError General

        do! checkMask w inputTx outputTx

        let chainedContract =
            message
            |> Option.map (fun message ->
                    {
                        sender = w.contractId
                        recipient = message.recipient
                        command = message.command
                        messageBody = message.body
                        beginInputs = w.beginInputs + w.inputsLength
                        beginOutputs = w.beginOutputs + w.outputsLength
                    })

        let! inputs = validateInputs w.contractId inputs w.inputsLength

        return inputs, chainedContract, contractStates
}

let private getSender (w:ContractWitness) txHash =
    let validateSignature (publicKey,signature) =
        match Crypto.verify publicKey signature txHash with
        | Crypto.Valid ->
            Ok (PKSender publicKey)
        | Crypto.Invalid ->
            GeneralError "invalid contract witness signature"

    Option.map validateSignature w.signature
    |> Option.defaultValue (Ok Anonymous)

let private resultToState witnesses result =
    match result with
    | Error error -> Invalid error
    | Ok (inputs, chainedContract, contractStates) ->
        match chainedContract with
        | Some chainedContract -> ExpectChainedContract (chainedContract, witnesses, inputs, contractStates)
        | None -> NextInput (witnesses, inputs, contractStates)

let validate blockNumber timestamp acs getContractState contractStates txHash tx witnesses inputs =
    match witnesses with
    | ContractWitness w :: witnesses ->
        getSender w txHash
        |> Result.bind (fun sender -> validateWitness {blockNumber=blockNumber;timestamp=timestamp} acs getContractState contractStates txHash tx sender inputs w)
        |> resultToState witnesses
    | _ -> 
        Invalid <| General "expecting a contract 0 witness"

let private isChainContract (w:ContractWitness) (chainContract:ChainedContractState) =
    w.contractId = chainContract.recipient &&
    w.beginInputs = chainContract.beginInputs &&
    w.beginOutputs = chainContract.beginOutputs &&
    w.command = chainContract.command &&
    w.messageBody = chainContract.messageBody &&
    Option.isNone w.signature

let validateChainedContract blockNumber timestamp acs getContractState contractStates txHash tx witnesses inputs chainedContract =
    match witnesses with
    | ContractWitness w :: witnesses when isChainContract w chainedContract ->
        let sender = ContractSender chainedContract.sender

        validateWitness {blockNumber=blockNumber;timestamp=timestamp} acs getContractState contractStates txHash tx sender inputs w
        |> resultToState witnesses

    | _ -> Invalid <| General "expecting chained contract witness"