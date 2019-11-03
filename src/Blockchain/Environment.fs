module Blockchain.Environment

open Consensus.Chain

type Env =
    {
        chainParams   : ChainParameters
        contractsPath : string
        timestamp     : uint64
        session       : DatabaseContext.Session
    }