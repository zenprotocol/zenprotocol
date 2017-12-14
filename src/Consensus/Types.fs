module Consensus.Types

open Consensus.Hash
open Consensus.Crypto

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

type Witness = 
    PKWitness of array<byte> * Signature

type Transaction = {
    inputs: Outpoint list
    outputs: Output list
    witnesses: Witness list
    contract: string Option
}