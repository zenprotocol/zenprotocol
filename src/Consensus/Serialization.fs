module Consensus.Serialization
open Consensus

open Consensus
open System
open Crypto
open FStar
open Types
open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

type TransactionSerializationMode =
    | Full
    | WithoutWitness

// Input discriminators
[<Literal>]
let private Outpoint = 1uy
// Spend discriminators (can be considered as a continuation of the Input discriminators list)
[<Literal>]
let private SpendZen = 2uy
[<Literal>]
let private SpendDefault = 3uy
[<Literal>]
let private Spend = 4uy

// Witness discriminators
[<Literal>]
let private PKWitness = 1uy
[<Literal>]
let private ContractWitness = 2uy

// Outputs discriminators
[<Literal>]
let private PK = 1uy
[<Literal>]
let private Contract = 2uy
[<Literal>]
let private Coinbase = 3uy
[<Literal>]
let private Fee = 4uy
[<Literal>]
let private ActivationSacrifice = 5uy
[<Literal>]
let private Destroy = 6uy

// Option discriminators
[<Literal>]
let private None = 0uy
[<Literal>]
let private Some = 1uy

type private Operations<'a> = {
    writeHash: Hash.Hash -> 'a -> 'a
    writeByte: Byte -> 'a -> 'a
    writeBytes: Byte[] -> int -> 'a -> 'a
    writeString: String -> 'a -> 'a
    writeLongString: String -> 'a -> 'a
    writeNumber4: uint32 -> 'a -> 'a
    writeNumber8: uint64 -> 'a -> 'a
}

let private writeByte byte =
    writeNumber1 byte
let private writeHash hash =
    writeBytes (Hash.bytes hash) Hash.Length
let private writeOption ops writerFn = function
    | Option.Some value ->
        ops.writeByte Some
        >> writerFn value
    | Option.None ->
        ops.writeByte None

let private serializers: Operations<Stream.T> = {
    writeHash = writeHash
    writeByte = writeByte
    writeBytes = writeBytes
    writeString = writeString
    writeLongString = writeLongString
    writeNumber4 = writeNumber4
    writeNumber8 = writeNumber8
}

let private counters: Operations<int32> = {
    writeHash = fun _ l -> l + Hash.Length
    writeByte = fun _ l -> l + 1
    writeBytes = fun _ len l -> l + len
    writeString = fun str l -> l + 1 + String.length str
    writeLongString = fun str l -> l + 4 + String.length str
    writeNumber4 = fun _ l -> l + 4
    writeNumber8 = fun _ l -> l + 8
}

let private writeSpend ops = function
    | { asset = cHash, token; amount = amount } when cHash = Hash.zero && token = Hash.zero ->
        ops.writeByte SpendZen
        >> ops.writeNumber8 amount
    | { asset = cHash, token; amount = amount } when cHash <> Hash.zero && token = Hash.zero ->
        ops.writeByte SpendDefault
        >> ops.writeHash cHash
        >> ops.writeNumber8 amount
    | { asset = cHash, token; amount = amount } when cHash <> Hash.zero && token <> Hash.zero ->
        ops.writeByte Spend
        >> ops.writeHash cHash
        >> ops.writeHash token
        >> ops.writeNumber8 amount
    | { asset = cHash, token; amount = _ } when cHash = Hash.zero && token <> Hash.zero ->
        failwithf "Not supported"
    | _ ->
        failwithf "Not expected"

let private writeInput ops = function
    | Types.Outpoint { txHash = txHash; index = index } ->
        ops.writeByte Outpoint
        >> ops.writeHash txHash
        >> ops.writeNumber4 index
    | Types.Mint spend ->
        writeSpend ops spend

let private writeWitness ops = function
    | Types.PKWitness (bytes, Signature signature) ->
        ops.writeByte PKWitness
        >> ops.writeBytes bytes Crypto.SerializedPublicKeyLength
        >> ops.writeBytes signature Crypto.SerializedSignatureLength
    | Types.ContractWitness cw ->
        let (Data data) = cw.data
        let dataLength = Array.length data
        ops.writeByte ContractWitness
        >> ops.writeHash cw.cHash
        >> ops.writeString cw.command
        >> ops.writeNumber4 (uint32 dataLength)
        >> ops.writeBytes data dataLength
        >> writeOption ops ops.writeNumber4 cw.returnAddressIndex
        >> ops.writeNumber4 cw.beginInputs
        >> ops.writeNumber4 cw.beginOutputs
        >> ops.writeNumber4 cw.inputsLength
        >> ops.writeNumber4 cw.outputsLength
        >> ops.writeNumber4 cw.cost
    | _ ->
        failwithf "Not expected"

let private writeLock ops = function
    | Types.PK hash ->
        ops.writeByte PK
        >> ops.writeHash hash
    | Types.Contract hash ->
        ops.writeByte Contract
        >> ops.writeHash hash
    | Types.Coinbase (blockNumber, pkHash) ->
        ops.writeByte Coinbase
        >> ops.writeNumber4 blockNumber
        >> ops.writeHash pkHash
    | Types.Fee ->
        ops.writeByte Fee
    | Types.ActivationSacrifice ->
        ops.writeByte ActivationSacrifice
    | Types.Destroy ->
        ops.writeByte Destroy

let private writeOutput ops = fun { lock = lock; spend = spend } ->
    writeLock ops lock
    >> writeSpend ops spend

let private writeContract ops =
    writeOption ops (fun (code, hints) ->
        ops.writeLongString code
        >> ops.writeLongString hints)

let private writeList ops writerFn list =
    let writeList list writerFn stream = //TODO: use fold?
        let mutable stream = stream
        for item in list do
            stream <- writerFn ops item stream
        stream
    ops.writeNumber4 (List.length list |> uint32)
    >> writeList list writerFn

let private writeTx mode ops tx =
    writeList ops writeInput tx.inputs
    >> writeList ops writeOutput tx.outputs
    >> writeContract ops tx.contract
    >> match mode with
    | Full -> writeList ops writeWitness tx.witnesses
    | WithoutWitness -> id

let serializeTransaction mode tx =
    writeTx mode counters tx 0
    |> create 
    |> writeTx mode serializers tx
    |> getBuffer

let serializeBlock bk =
    let writeCommitment ops hash = ops.writeHash hash
    let writeTransaction ops tx = writeTx Full ops tx

    let writeBk ops =
        ops.writeNumber4 bk.header.version
        >> ops.writeHash bk.header.parent
        >> ops.writeNumber4 bk.header.blockNumber
        >> ops.writeHash bk.header.commitments
        >> ops.writeNumber8 bk.header.timestamp
        >> ops.writeNumber4 bk.header.difficulty
        >> ops.writeNumber8 (fst bk.header.nonce)
        >> ops.writeNumber8 (snd bk.header.nonce)
        >> writeList ops writeCommitment ([ bk.txMerkleRoot; bk.witnessMerkleRoot; bk.activeContractSetMerkleRoot ] @ bk.commitments)
        >> writeList ops writeTransaction bk.transactions

    writeBk counters 0
    |> create
    |> writeBk serializers
    |> getBuffer

// Deserialization 

let readByte =
    readNumber1
let readHash =
    readBytes Hash.Length
let readList readerFn = reader {
    let! length = readNumber4
    let! list = reader {
        for _ in [1..int length] do
            let! item = readerFn
            return item
    }
    return List.ofSeq list
}

let private readSpendZen = reader {
    let! amount = readNumber8
    return { asset = Constants.Zen; amount = amount }
}

let private readSpendDefault = reader {
    let! cHash = readHash
    let! amount = readNumber8
    return { asset = Hash.Hash cHash, Hash.zero; amount = amount }
}

let private readSpendInner = reader {
    let! cHash = readHash
    let! token = readHash
    let! amount = readNumber8
    return { asset = Hash.Hash cHash, Hash.Hash token; amount = amount }
}

let private readSpend = reader {
    let! discriminator = readByte
    match discriminator with
    | SpendZen ->
        let! spend = readSpendZen
        return spend
    | SpendDefault ->
        let! spend = readSpendDefault
        return spend
    | Spend ->
        let! spend = readSpendInner
        return spend
    | _ ->
        return raise <| Exception "Not expected: spend discriminator"
}

let private readInput = reader {
    let! discriminator = readByte
    match discriminator with
    | Outpoint->
        let! txHash = readHash
        let! index = readNumber4
        return Types.Outpoint { txHash = Hash.Hash txHash; index = index }
    | SpendZen ->
        let! spend = readSpendZen
        return Mint spend
    | SpendDefault ->
        let! spend = readSpendDefault
        return Mint spend
    | Spend ->
        let! spend = readSpendInner
        return Mint spend
    | _ ->
        return raise <| Exception "Not expected: input discriminator"
}

let private readOption readerFn =reader {
    let! discriminator = readByte
    
    match discriminator with
    | Some ->
        let! item = readerFn
        return Option.Some item
    | None ->
        return Option.None
    | _ ->
        return raise <| Exception "Not expected: option discriminator"
}

let private readWitness = reader {
    let! discriminator = readByte
    match discriminator with
    | PKWitness ->
        let! publicKey = readBytes Crypto.SerializedPublicKeyLength
        let! signature = readBytes Crypto.SerializedSignatureLength
        return Types.PKWitness (publicKey, Signature signature)
    | ContractWitness ->
        let! cHash = readHash
        let! command = readString
        let! dataLength = readNumber4
        let! data = readBytes (int dataLength)
        let! returnAddressIndex = readOption readNumber4
        let! beginInputs = readNumber4
        let! beginOutputs = readNumber4
        let! inputsLength = readNumber4
        let! outputsLength = readNumber4
        let! cost = readNumber4
        return Types.ContractWitness {
            cHash = Hash.Hash cHash
            command = command
            data = Data data
            returnAddressIndex = returnAddressIndex
            beginInputs = beginInputs
            beginOutputs = beginOutputs
            inputsLength = inputsLength
            outputsLength = outputsLength
            cost = cost
        }
    | _ ->
        return raise <| Exception "Not expected: witness discriminator"
}

let private readLock = reader {
    let! discriminator = readByte
    match discriminator with
    | PK ->
        let! hash = readHash
        return Lock.PK (Hash.Hash hash)
    | Contract ->
        let! hash = readHash
        return Lock.Contract (Hash.Hash hash)
    | Coinbase ->
        let! blockNumber = readNumber4
        let! pkHash = readHash
        return Lock.Coinbase (blockNumber, Hash.Hash pkHash)
    | Fee ->
        return Lock.Fee
    | ActivationSacrifice ->
        return Lock.ActivationSacrifice
    | Destroy ->
        return Lock.Destroy
    | _ ->
        return raise <| Exception "Not expected: output discriminator"
}

let private readOutput = reader {
    let! lock = readLock
    let! spend = readSpend
    return { lock = lock; spend = spend }
}

let private readContract = reader {
    let readContract = reader {
        let! code = readLongString
        let! hints = readLongString
        return code, hints
    }
    let! contract = readOption readContract
    return contract
}

let private readTx mode = reader {
    let! inputs = readList readInput
    let! outputs = readList readOutput
    let! contract = readContract
    let! witnesses = reader {
        match mode with
        | Full -> yield! readList readWitness
        | WithoutWitness -> return []
    }
    return { inputs = inputs; witnesses = witnesses; outputs = outputs; contract = contract }
}

let deserializeTransaction mode bytes =
    Stream (bytes, 0)
    |> run (readTx mode)
    
let deserializeBlock bytes =
    let readBk = reader {
        let! version = readNumber4
        let! parent = readHash
        let! blockNumber = readNumber4
        let! commitments = readHash
        let! timestamp = readNumber8
        let! difficulty = readNumber4
        let! nonceFst = readNumber8
        let! nonceSnd = readNumber8
        let! commitmentsList = readList readHash
        let! transactions = readList (readTx Full)
    
        let blockHeader = {
            version = version
            parent = Hash.Hash parent
            blockNumber = blockNumber
            commitments = Hash.Hash commitments
            timestamp = timestamp
            difficulty = difficulty
            nonce = nonceFst, nonceSnd
        }
        
        return {
            header = blockHeader
            txMerkleRoot = Hash.Hash commitmentsList.[0]
            witnessMerkleRoot = Hash.Hash commitmentsList.[1]
            activeContractSetMerkleRoot = Hash.Hash commitmentsList.[2]
            commitments = List.map Hash.Hash commitmentsList.[3 .. List.length commitmentsList - 1]
            transactions = transactions
        }
    }
    
    Stream (bytes, 0)
    |> run readBk