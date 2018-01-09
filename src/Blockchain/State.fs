module Blockchain.State

open Consensus
open Consensus.Types

type BlockState = 
    {
        tip: PersistentBlock.T
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
        blockRepository:BlockRepository.T        
        tipState: BlockState
        memoryState: MemoryState
        blockRequests: Set<Hash.Hash>             
    }