module Consensus.InputValidation.State

open Consensus
open ValidationError
open Types
open TxSkeleton
open Zen.Types.Data

type ChainedContractState = {
    sender: ContractId
    recipient: ContractId
    command: string
    messageBody: data option
    beginInputs: uint32
    beginOutputs: uint32
}

type T =
    | Invalid of ValidationError
    | NextInput of Witness list * TxSkeleton.Input list * ContractStates.T
    | ExpectChainedContract of ChainedContractState * Witness list * TxSkeleton.Input list * ContractStates.T
    | Valid of ContractStates.T