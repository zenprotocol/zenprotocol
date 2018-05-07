module Consensus.InputValidation.ContractV0

open Consensus
open Consensus
open Consensus.Types
open Consensus.ValidationError
open Consensus.TxSkeleton
open State


let private validateCost contract initialTx sender contractWallet (w:ContractWitness) =
    let cost = Contract.getCost contract initialTx w.command sender w.data contractWallet

    if uint32 cost <> w.cost then
        GeneralError (sprintf "Contract witness committed to cost %d, but cost of execution is %d" (uint32 cost) w.cost)
    else
        Ok ()

let private checkMask w inputTx outputTx =
    if List.length outputTx.pInputs - List.length inputTx.pInputs = int w.inputsLength &&
       List.length outputTx.outputs - List.length inputTx.outputs = int w.outputsLength then
        Ok ()
    else
        GeneralError "input/output length mismatch"

let rec private validateInputs contractId inputs amount =
    match inputs, amount with
    | inputs, 0ul -> Ok inputs
    | [], _ -> GeneralError "input/output length mismatch"
    | input :: inputs, amount ->
        match input with
        | Mint spend ->
            let contractId' = Asset.contractId spend.asset

            if contractId' <> contractId then
                GeneralError "illegal creation of tokens"
            else
                validateInputs contractId inputs (amount - 1ul)
        | PointedOutput (_,output) ->
            match output.lock with
            | Contract contractId' when contractId' = contractId ->
                validateInputs contractId inputs (amount - 1ul)
            | _ -> GeneralError "cannot unlock input"

let private validateWitness acs txHash finalTx sender inputs (w:ContractWitness) = result {
    match ActiveContractSet.tryFind w.contractId acs with
    | None ->
        return! Error ContractNotActive
    | Some contract ->
        // apply the wintess mask on the transaction
        let! inputTx =
            TxSkeleton.applyMask finalTx w
            |> Result.mapError General

        let contractWallet = Contract.getContractWallet finalTx w

        do! validateCost contract inputTx sender contractWallet w

        // running the contract
        let! outputTx,message =
             Contract.run contract inputTx w.command sender w.data contractWallet
             |> Result.mapError General

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
                        recipient = message.contractId
                        command = message.command
                        data = message.data
                        beginInputs = w.beginInputs + w.inputsLength
                        beginOutputs = w.beginOutputs + w.outputsLength
                    })

        let! inputs = validateInputs w.contractId inputs w.inputsLength

        return inputs,chainedContract
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
    | Ok (inputs, chainedContract) ->
        match chainedContract with
        | Some chainedContract -> ExpectChainedContract (chainedContract, witnesses, inputs)
        | None -> NextInput (witnesses, inputs)

let validate acs txHash tx witnesses inputs =
    match witnesses with
    | ContractWitness w :: witnesses ->
        getSender w txHash
        |> Result.bind (fun sender -> validateWitness acs txHash tx sender inputs w)
        |> resultToState witnesses
    | _ -> Invalid <| General "expecting a contract 0 witness"

let private isChainContract (w:ContractWitness) (chainContract:ChainedContractState) =
    w.contractId = chainContract.recipient &&
    w.beginInputs = chainContract.beginInputs &&
    w.beginOutputs = chainContract.beginOutputs &&
    w.command = chainContract.command &&
    w.data = chainContract.data &&
    Option.isNone w.signature

let validateChainedContract acs txHash tx witnesses inputs chainedContract  =
    match witnesses with
    | ContractWitness w :: witnesses when isChainContract w chainedContract ->
        let sender = ContractSender chainedContract.sender

        validateWitness acs txHash tx sender inputs w
        |> resultToState witnesses

    | _ -> Invalid <| General "expecting chained contract witness"