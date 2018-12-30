module Consensus.InputValidation.StateMachine

open Consensus
open ValidationError
open Types
open TxSkeleton
open State

let private validateInputs chainParams blockNumber timestamp acs getContractState contractStates (txHash:Hash.Hash) tx witnesses (inputs:TxSkeleton.Input list) =
    match inputs with
    | [] -> Valid contractStates
    | Lock head :: tail ->
        match head with
        | Contract contractId ->
            if ContractId.version contractId <> Version0 then
                Invalid <| General "contract version not supported"
            else
                ContractV0.validate blockNumber timestamp acs getContractState contractStates txHash tx witnesses inputs
        | PK pkHash
        | Vote (_, _, pkHash) -> PK.validate contractStates txHash witnesses pkHash tail
        | Coinbase (coinbaseBlockNumber, pkHash) ->
            Coinbase.validate chainParams blockNumber contractStates txHash witnesses pkHash coinbaseBlockNumber tail
        | Fee
        | ActivationSacrifice
        | ExtensionSacrifice _
        | Destroy
        | HighVLock _ -> Invalid <| General "input is not spendable"

let rec private validateNext chainParams blockNumber timestamp acs (txHash:Hash.Hash) tx getContractState state =
    match state with
    | Invalid err -> Invalid err
    | NextInput (witnesses, inputs, contractStates) ->
        validateInputs chainParams blockNumber timestamp acs getContractState contractStates txHash tx witnesses inputs
        |> validateNext chainParams blockNumber timestamp acs txHash tx getContractState
    | ExpectChainedContract (chainedContract, witnesses, inputs, contractStates) ->
        if ContractId.version chainedContract.recipient <> Version0 then
            Invalid <| General "contract version not supported"
        else
            ContractV0.validateChainedContract blockNumber timestamp acs getContractState contractStates txHash tx witnesses inputs chainedContract
            |> validateNext chainParams blockNumber timestamp acs txHash tx getContractState
    | Valid contractState -> Valid contractState

let validate chainParams blockNumber timestamp acs getContractState contractState txHash tx txSkeleton =
    let witnesses = tx.witnesses
    let inputs = txSkeleton.pInputs

    match validateNext chainParams blockNumber timestamp acs txHash txSkeleton getContractState (NextInput (witnesses,inputs,contractState)) with
    | Valid stateUpdate -> Ok stateUpdate
    | Invalid error -> Error error
    | ExpectChainedContract _ -> Error <| General "chained contract witness expected"
    | NextInput _ -> failwith "unexpected"
