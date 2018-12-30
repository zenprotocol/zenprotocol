module Blockchain.BlockState

open Consensus
open Consensus.Types

type T =
    {
        ema:EMA.T
        cgp:CGP.T
        activeContractSetUndoData:ActiveContractSet.UndoData
        contractStatesUndoData:ContractStates.UndoData
    }