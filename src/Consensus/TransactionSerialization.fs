module Consensus.TransactionSerialization
open Consensus

open Consensus
open System
open Crypto
open FStar
open Types
open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

type SerializationMode =
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

type private fns<'a> = {
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
let private writeOption fns writerFn = function
    | Option.Some value ->
        fns.writeByte Some
        >> writerFn value
    | Option.None ->
        fns.writeByte None

let private serializers: fns<Stream.T> = {
    writeHash = writeHash
    writeByte = writeByte
    writeBytes = writeBytes
    writeString = writeString
    writeLongString = writeLongString
    writeNumber4 = writeNumber4
    writeNumber8 = writeNumber8
}

let private counters: fns<int32> = {
    writeHash = fun _ l -> l + Hash.Length
    writeByte = fun _ l -> l + 1
    writeBytes = fun _ len l -> l + len
    writeString = fun str l -> l + 1 + String.length str
    writeLongString = fun str l -> l + 4 + String.length str
    writeNumber4 = fun _ l -> l + 4
    writeNumber8 = fun _ l -> l + 8
}

let serialize mode tx =
    let writeSpend fns = function
        | { asset = cHash, token; amount = amount } when cHash = Hash.zero && token = Hash.zero ->
            fns.writeByte SpendZen
            >> fns.writeNumber8 amount
        | { asset = cHash, token; amount = amount } when cHash <> Hash.zero && token = Hash.zero ->
            fns.writeByte SpendDefault
            >> fns.writeHash cHash
            >> fns.writeNumber8 amount
        | { asset = cHash, token; amount = amount } when cHash <> Hash.zero && token <> Hash.zero ->
            fns.writeByte Spend
            >> fns.writeHash cHash
            >> fns.writeHash token
            >> fns.writeNumber8 amount
        | { asset = cHash, token; amount = _ } when cHash = Hash.zero && token <> Hash.zero ->
            failwithf "Not supported"
        | _ ->
            failwithf "Not expected"

    let writeInput fns = function
        | Types.Outpoint { txHash = txHash; index = index } ->
            fns.writeByte Outpoint
            >> fns.writeHash txHash
            >> fns.writeNumber4 index
        | Types.Mint spend ->
            writeSpend fns spend

    let writeWitness fns = function
        | Types.PKWitness (bytes, Signature signature) ->
            fns.writeByte PKWitness
            >> fns.writeBytes bytes Crypto.SerializedPublicKeyLength
            >> fns.writeBytes signature Crypto.SerializedSignatureLength
        | Types.ContractWitness cw ->
            let (Data data) = cw.data
            let dataLength = Array.length data
            fns.writeByte ContractWitness
            >> fns.writeHash cw.cHash
            >> fns.writeString cw.command
            >> fns.writeNumber4 (uint32 dataLength)
            >> fns.writeBytes data dataLength
            >> writeOption fns fns.writeNumber4 cw.returnAddressIndex
            >> fns.writeNumber4 cw.beginInputs
            >> fns.writeNumber4 cw.beginOutputs
            >> fns.writeNumber4 cw.inputsLength
            >> fns.writeNumber4 cw.outputsLength
            >> fns.writeNumber4 cw.cost
        | _ ->
            failwithf "Not expected"

    let writeLock fns = function
        | Types.PK hash ->
            fns.writeByte PK
            >> fns.writeHash hash
        | Types.Contract hash ->
            fns.writeByte Contract
            >> fns.writeHash hash
        | Types.Coinbase (blockNumber, pkHash) ->
            fns.writeByte Coinbase
            >> fns.writeNumber4 blockNumber
            >> fns.writeHash pkHash
        | Types.Fee ->
            fns.writeByte Fee
        | Types.ActivationSacrifice ->
            fns.writeByte ActivationSacrifice
        | Types.Destroy ->
            fns.writeByte Destroy

    let writeOutput fns = fun { lock = lock; spend = spend } ->
        writeLock fns lock
        >> writeSpend fns spend

    let writeContract fns =
        writeOption fns (fun (code, hints) ->
            fns.writeLongString code
            >> fns.writeLongString hints)

    let writeItem fns item =
        match box item with
        | (:? Input as input) -> writeInput fns input
        | (:? Witness as witness) -> writeWitness fns witness
        | (:? Output as output) -> writeOutput fns output
        | _ -> failwithf "Not expected"

    let writeList fns list =
        let writeList list writerFn stream = //TODO: use fold?
            let mutable stream = stream
            for item in list do
                stream <- writerFn fns item stream
            stream
        fns.writeNumber4 (List.length list |> uint32)
        >> writeList list writeItem

    // main serialization function
    let writeTx fns tx =
        writeList fns tx.inputs
        >> writeList fns tx.outputs
        >> writeContract fns tx.contract
        >> match mode with
        | Full -> writeList fns tx.witnesses
        | WithoutWitness -> id

    writeTx counters tx 0 // get the byte count
    |> create // create a buffer with the byte count and do the actual serialization
    |> writeTx serializers tx
    |> getBuffer

let deserialize bytes =
    let stream = Stream (bytes, 0)

    let readByte =
        readNumber1
    let readHash =
        readBytes Hash.Length

    let readSpendZen = reader {
        let! amount = readNumber8
        return { asset = Constants.Zen; amount = amount }
    }

    let readSpendDefault = reader {
        let! cHash = readHash
        let! amount = readNumber8
        return { asset = Hash.Hash cHash, Hash.zero; amount = amount }
    }

    let readSpendInner = reader {
        let! cHash = readHash
        let! token = readHash
        let! amount = readNumber8
        return { asset = Hash.Hash cHash, Hash.Hash token; amount = amount }
    }

    let readSpend = reader {
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

    let readInput = reader {
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

    let readOption readerFn =reader {
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

    let readWitness = reader {
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

    let readLock = reader {
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

    let readOutput = reader {
        let! lock = readLock
        let! spend = readSpend
        return { lock = lock; spend = spend }
    }

    let readContract = reader {
        let readContract = reader {
            let! code = readLongString
            let! hints = readLongString
            return code, hints
        }
        let! contract = readOption readContract
        return contract
    }

    let readList readerFn = reader {
        let! length = readNumber4
        let! list = reader {
            for _ in [1..int length] do
                let! item = readerFn
                return item
        }
        return List.ofSeq list
    }

    // main deserialization function
    let readTx = reader {
        let! inputs = readList readInput
        let! witnesses = readList readWitness
        let! outputs = readList readOutput
        let! contract = readContract
        return { inputs = inputs; witnesses = witnesses; outputs = outputs; contract = contract }
    }

    run readTx stream