module Consensus.Types

open System
open Consensus.Hash
open Consensus.Crypto
open Zen.Types.Data

[<LiteralAttribute>]
let CoinbaseMaturity = 100ul

type Outpoint = {
    txHash: Hash
    index: uint32
}

type Asset = Hash * Hash

type Spend = {
    asset: Asset
    amount: uint64
}

type Input =
    | Outpoint of Outpoint
    | Mint of Spend

type Lock =
    | PK of Hash
    | Contract of Hash
    | Coinbase of blockNumber:uint32 * pkHash:Hash
    | Fee
    | ActivationSacrifice
    | Destroy

type Output = {
    lock: Lock
    spend: Spend
}

type PointedOutput = Outpoint * Output

type Message = {
    cHash: Hash
    command: string
    data: data option
}

type ContractWitness =
    {
        cHash: Hash
        command: string
        data: data option
        beginInputs: uint32
        beginOutputs: uint32
        inputsLength: uint32
        outputsLength: uint32
        cost: uint32
    }
    with
        member x.endOutputs = x.beginOutputs + x.outputsLength - 1ul
        member x.endInputs = x.beginInputs + x.inputsLength - 1ul

type Witness =
    | PKWitness of array<byte> * Signature
    | ContractWitness of ContractWitness

type Transaction = {
    inputs: Input list
    outputs: Output list
    witnesses: Witness list
    contract: (string * string) Option
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
    txMerkleRoot:Hash.Hash;
    witnessMerkleRoot:Hash.Hash;
    activeContractSetMerkleRoot:Hash.Hash;
    commitments: Hash.Hash list;
    transactions:Transaction list;
}