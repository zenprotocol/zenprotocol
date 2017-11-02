module Consensus.Types

type Hash = byte[]

type Outpoint = {
    txHash: Hash; 
    index: uint32
}

type Transaction = {
    inputs: Outpoint list;
    witnesses: byte[] list
}