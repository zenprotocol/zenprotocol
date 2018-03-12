module Blockchain.State

open Consensus

type BlockRequest =
    | ParentBlock
    | Tip
    | NewBlock

type TipState =
    {
        tip: ExtendedBlockHeader.T
        activeContractSet: ActiveContractSet.T
        ema:EMA.T
    }

type MemoryState =
    {
        utxoSet: UtxoSet.T
        activeContractSet: ActiveContractSet.T
        orphanPool: OrphanPool.T
        mempool: MemPool.T
    }

type State =
    {
        tipState: TipState
        memoryState: MemoryState
        blockRequests: Map<Hash.Hash, BlockRequest>
        headers: uint32
    }