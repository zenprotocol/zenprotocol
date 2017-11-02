module Consensus.Types

open Consensus.Hash

type Outpoint = {
    txHash: Hash; 
    index: uint32
}

type Transaction = {
    inputs: Outpoint list;
    witnesses: byte[] list
}