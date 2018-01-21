module Blockchain.State

open Consensus
open Consensus.Types

type BlockRequest = 
    | ParentBlock 
    | Tip
    | NewBlock

type BlockState = 
    {
        tip: ExtendedBlockHeader.T
        utxoSet: UtxoSet.T 
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
        tipState: BlockState
        memoryState: MemoryState
        blockRequests: Map<Hash.Hash, BlockRequest>             
    }