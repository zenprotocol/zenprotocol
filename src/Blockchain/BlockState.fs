module Blockchain.BlockState

open Consensus

type T =
    {
        ema:EMA.T
        activeContractSet:(Hash.Hash*uint32*string) list
    }
