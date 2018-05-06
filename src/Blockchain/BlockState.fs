module Blockchain.BlockState

open Consensus
open Consensus.Types

type ContractState = {
    contractId: ContractId
    expiry:uint32
    code:string
}

type T =
    {
        ema:EMA.T
        activeContractSet:ContractState list
    }
