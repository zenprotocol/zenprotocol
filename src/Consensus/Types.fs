module Consensus.Types

open Consensus.Hash

type Outpoint = {
    txHash: Hash
    index: uint32
}

type Spend = {
    asset: Hash
    amount: uint64
}

type Lock =
    | PK of Hash

type Output = {
    lock: Lock
    spend: Spend
}

type Transaction = {
    inputs: Outpoint list;
    outputs: Output list;
    witnesses: byte[] list
}