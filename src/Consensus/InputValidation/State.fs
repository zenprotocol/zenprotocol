module Consensus.InputValidation.State

open Consensus
open Consensus.ValidationError
open Consensus.Types
open Consensus.TxSkeleton
open Zen.Types.Data

type ChainedContractState = {
    sender: ContractId
    recipient: ContractId
    command: string
    data: data option
    beginInputs:uint32
    beginOutputs:uint32
}

type T =
    | Invalid of ValidationError
    | NextInput of Witness list * TxSkeleton.Input list
    | ExpectChainedContract of ChainedContractState * Witness list * TxSkeleton.Input list
    | Valid