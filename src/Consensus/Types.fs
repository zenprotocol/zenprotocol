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
    | Contract of Hash * array<byte>

type Output = {
    lock: Lock
    spend: Spend
}

type Witness = 
    | PKWitness of array<byte> * Signature
    | ContractWitness of 
        Hash * 
        beginInputs:int * 
        beginOutputs:int * 
        endInputs:int * 
        endOutputs:int

type Transaction = {
    inputs: Outpoint list
    outputs: Output list
    witnesses: Witness list
    contract: string Option
}

type Nonce = uint64 * uint64

type BlockHeader = {
    version: uint32;
    parent: Hash.Hash;
    blockNumber: uint32;
    commitments: Hash.Hash;
    timestamp: uint64;
    difficulty: uint32;
    nonce: Nonce;
}

type Block = {
    header:BlockHeader;
    transactions:Transaction list;
}