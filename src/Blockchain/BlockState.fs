module Blockchain.BlockState

open Consensus

type T =
    {
        ema:EMA.T
        cgp:CGP.T
        activeContractSetUndoData:ActiveContractSet.UndoData
        contractStatesUndoData:ContractStates.UndoData
    }