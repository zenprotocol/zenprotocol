module Blockchain.State

open Consensus
open Types

   
type TipState =
    {
        tip: ExtendedBlockHeader.T
        activeContractSet: ActiveContractSet.T
        ema: EMA.T
        cgp: CGP.T
    }

type MemoryState =
    {
        utxoSet: UtxoSet.T
        activeContractSet: ActiveContractSet.T
        orphanPool: OrphanPool.T
        mempool: MemPool.T
        contractCache: ContractCache.T
        contractStates: ContractStates.T
        invalidTxHashes: Set<Hash.Hash>
    }

type State =
    {
        tipState: TipState
        memoryState: MemoryState
        initialBlockDownload:InitialBlockDownload.T
        headers: uint32
    }
