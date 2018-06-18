module Blockchain.BlockState

open Consensus
open Consensus.Types

type T =
    {
        ema:EMA.T
        activeContractSetUndoData:ActiveContractSet.UndoData
        contractStatesUndoData:ContractStates.UndoData
    }