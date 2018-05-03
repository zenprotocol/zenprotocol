module Consensus.InputValidation.StateMachine

open Consensus
open Consensus.ValidationError
open Consensus.Types
open Consensus.TxSkeleton

open State

let private validateInputs blockNumber timestamp acs txHash tx witnesses (inputs:TxSkeleton.Input list) =
    match inputs with
    | [] -> Valid
    | head :: tail ->
        match head with
        | Lock (Contract contractId) ->
            if ContractId.version contractId <> Version0 then
                Invalid <| General "contract version not supported"
            else
                ContractV0.validate blockNumber timestamp acs txHash tx witnesses inputs
        | Lock (PK pkHash) -> PK.validate txHash witnesses pkHash tail
        | Lock (Coinbase (coinbaseBlockNumber, pkHash)) ->
            Coinbase.validate blockNumber txHash witnesses pkHash coinbaseBlockNumber tail
        | Lock Fee
        | Lock ActivationSacrifice
        | Lock (ExtensionSacrifice _)
        | Lock Destroy
        | Lock (HighVLock _) -> Invalid <| General "input is not spendable"

let rec private validateNext blockNumber timestamp acs txHash tx state =
    match state with
    | Invalid err -> Invalid err
    | NextInput (witnesses, inputs) ->
        validateInputs blockNumber timestamp acs txHash tx witnesses inputs
        |> validateNext blockNumber timestamp acs txHash tx
    | ExpectChainedContract (chainedContract,witnesses,inputs) ->
        if ContractId.version chainedContract.recipient <> Version0 then
            Invalid <| General "contract version not supported"
        else
            ContractV0.validateChainedContract blockNumber timestamp acs txHash tx witnesses inputs chainedContract
            |> validateNext blockNumber timestamp acs txHash tx
    | Valid -> Valid

let validate blockNumber timestamp acs pInputs txHash tx txSkeleton =
    let witnesses = tx.witnesses
    let inputs = txSkeleton.pInputs

    match validateNext blockNumber timestamp acs txHash txSkeleton (NextInput (witnesses,inputs)) with
    | Valid -> Ok ()
    | Invalid error -> Error error
    | ExpectChainedContract _ -> Error <| General "chained contract witness expected"
    | NextInput _ -> failwith "unexpected"


