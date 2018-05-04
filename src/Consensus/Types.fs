module Consensus.Types

open System
open Consensus.Hash
open Consensus.Crypto
open Zen.Types.Data
open Infrastructure
open FsBech32

[<LiteralAttribute>]
let CoinbaseMaturity = 100ul

[<Literal>]
let Version0 = 0ul

type Outpoint = {
    txHash: Hash
    index: uint32
}

[<StructuredFormatDisplay("{AsString}")>]
type ContractId = ContractId of uint32 * Hash with
    override x.ToString() =
        let (ContractId (version,cHash)) = x

        Array.append (BigEndianBitConverter.uint32ToBytes version) (Hash.bytes cHash)
        |> Base16.encode

    member x.AsString = x.ToString()

[<StructuredFormatDisplay("{AsString}")>]
type Asset = Asset of ContractId * Hash with
   override x.ToString() =
        let (Asset (contractId,subType)) = x

        // Check if Zen asset
        if contractId = (ContractId (Version0,Hash.zero)) && subType = Hash.zero then
            "00"
        elif subType = Hash.zero then
            contractId.AsString
        else
            sprintf "%s%s" (contractId.AsString) (subType.AsString)

   member x.AsString = x.ToString()

type Spend = {
    asset: Asset
    amount: uint64
}

type Input =
    | Outpoint of Outpoint
    | Mint of Spend

type Lock =
    | PK of Hash
    | Contract of ContractId
    | Coinbase of blockNumber:uint32 * pkHash:Hash
    | Fee
    | ActivationSacrifice
    | ExtensionSacrifice of ContractId
    | Destroy
    | HighVLock of identifier:uint32 * byte[]

type Output = {
    lock: Lock
    spend: Spend
}

type PointedOutput = Outpoint * Output

type Message = {
    contractId: ContractId
    command: string
    data: data option
}

type ContractWitness =
    {
        contractId: ContractId
        command: string
        data: data option
        beginInputs: uint32
        beginOutputs: uint32
        inputsLength: uint32
        outputsLength: uint32
        signature: (PublicKey * Signature) option
        cost: uint32
    }
    with
        member x.endOutputs = x.beginOutputs + x.outputsLength - 1ul
        member x.endInputs = x.beginInputs + x.inputsLength - 1ul

type Witness =
    | PKWitness of PublicKey * Signature
    | ContractWitness of ContractWitness
    | HighVWitness of identifier:uint32 * byte[]

type ContractV0 = {
    code: string
    hints: string
    rlimit: uint32
    queries: uint32
}

type Contract =
    | V0 of ContractV0
    | HighV of version:uint32 * byte[]

type Transaction = {
    version: uint32
    inputs: Input list
    outputs: Output list
    witnesses: Witness list
    contract: Contract Option
}

type Nonce = uint64 * uint64

type BlockHeader = {
    version: uint32
    parent: Hash.Hash
    blockNumber: uint32
    commitments: Hash.Hash
    timestamp: uint64
    difficulty: uint32
    nonce: Nonce
}

type Block = {
    header: BlockHeader
    txMerkleRoot: Hash.Hash
    witnessMerkleRoot: Hash.Hash
    activeContractSetMerkleRoot: Hash.Hash
    commitments: Hash.Hash list
    transactions: Transaction list
}

let Anonymous = Zen.Types.Main.Anonymous
let ContractSender (ContractId (version, Hash.Hash cHash)) = Zen.Types.Main.Contract (version,cHash)
let PKSender (Crypto.PublicKey publicKey) = Zen.Types.Main.PK publicKey