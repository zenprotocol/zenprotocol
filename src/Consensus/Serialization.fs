module Consensus.Serialization
open Consensus

open Consensus
open System
open Crypto
open Types
open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

[<Literal>]
let SerializedHeaderSize = 100 

type TransactionSerializationMode =
    | Full
    | WithoutWitness

module private Serialization = 
    module Hash = 
        let write hash = writeBytes (Hash.bytes hash) Hash.Length
        let read = readBytes Hash.Length
    
    module Byte =
        let write byte = writeNumber1 byte
        let read = readNumber1
    
    type Operations<'a> = {
        writeHash: Hash.Hash -> 'a -> 'a
        writeByte: Byte -> 'a -> 'a
        writeBytes: Byte[] -> int -> 'a -> 'a
        writeString: String -> 'a -> 'a
        writeLongString: String -> 'a -> 'a
        writeNumber4: uint32 -> 'a -> 'a
        writeNumber8: uint64 -> 'a -> 'a
    }
    
    let serializers: Operations<Stream.T> = {
        writeHash = Hash.write
        writeByte = Byte.write
        writeBytes = writeBytes
        writeString = writeString
        writeLongString = writeLongString
        writeNumber4 = writeNumber4
        writeNumber8 = writeNumber8
    }
    
    let counters: Operations<int32> = {
        writeHash = fun _ l -> l + Hash.Length
        writeByte = fun _ l -> l + 1
        writeBytes = fun _ len l -> l + len
        writeString = fun str l -> l + 1 + String.length str
        writeLongString = fun str l -> l + 4 + String.length str
        writeNumber4 = fun _ l -> l + 4
        writeNumber8 = fun _ l -> l + 8
    }
    
    let fail stream =
        None, stream 
        
    module Option =
        [<Literal>]
        let private None = 0uy
        [<Literal>]
        let private Some = 1uy
    
        let write ops writerFn = function
            | Option.Some value ->
                ops.writeByte Some
                >> writerFn value
            | Option.None ->
                ops.writeByte None
        let read readerFn =reader {
            let! discriminator = Byte.read
            match discriminator with
            | Some ->
                let! item = readerFn
                return Option.Some item
            | None ->
                return Option.None
            | _ ->
                yield! fail
        }
    
    module List =
        let read readerFn = reader {
            let! length = readNumber4
            let! list = reader {
                for _ in [1..int length] do
                    let! item = readerFn
                    return item
            }
            return List.ofSeq list
        }
        let write ops writerFn list =
            let write list writerFn stream = //TODO: use fold?
                let mutable stream = stream //TODO: this is not 'stream' if ops are Counters
                for item in list do
                    stream <- writerFn ops item stream
                stream
            ops.writeNumber4 (List.length list |> uint32)
            >> write list writerFn
    
    module Asset = 
        let write ops = fun (cHash, token) ->
            ops.writeHash cHash
            >> ops.writeHash token
        
        let read = reader {
            let! cHash = Hash.read
            let! token = Hash.read
            return Hash.Hash cHash, Hash.Hash token
        }
        
    module Spend =
        let write ops = fun { asset = asset; amount = amount } ->
            Asset.write ops asset
            >> ops.writeNumber8 amount
        let read = reader {
            let! asset = Asset.read
            let! amount = readNumber8
            return { asset = asset; amount = amount }
        }
        
    module Input =
        [<Literal>]
        let private SerializedOutpoint = 1uy
        [<Literal>]
        let private SerializedMint = 2uy
    
        let write ops = function
            | Outpoint { txHash = txHash; index = index } ->
                ops.writeByte SerializedOutpoint
                >> ops.writeHash txHash
                >> ops.writeNumber4 index
            | Mint spend ->
                ops.writeByte SerializedMint
                >> Spend.write ops spend
        let read = reader {
            let! discriminator = Byte.read
            match discriminator with
            | SerializedOutpoint ->
                let! txHash = Hash.read
                let! index = readNumber4
                return Outpoint { txHash = Hash.Hash txHash; index = index }
            | SerializedMint ->
                let! spend = Spend.read
                return (Mint spend)
            | _ ->
                yield! fail
        }
    
    module Witness =
        [<Literal>]
        let private SerializedPKWitness = 1uy
        [<Literal>]
        let private SerializedContractWitness = 2uy
    
        let write ops = function
            | PKWitness (bytes, Signature signature) ->
                ops.writeByte SerializedPKWitness
                >> ops.writeBytes bytes Crypto.SerializedPublicKeyLength
                >> ops.writeBytes signature Crypto.SerializedSignatureLength
            | ContractWitness cw ->
                let (Data data) = cw.data
                let dataLength = Array.length data
                ops.writeByte SerializedContractWitness
                >> ops.writeHash cw.cHash
                >> ops.writeString cw.command
                >> ops.writeNumber4 (uint32 dataLength)
                >> ops.writeBytes data dataLength
                >> Option.write ops ops.writeNumber4 cw.returnAddressIndex
                >> ops.writeNumber4 cw.beginInputs
                >> ops.writeNumber4 cw.beginOutputs
                >> ops.writeNumber4 cw.inputsLength
                >> ops.writeNumber4 cw.outputsLength
                >> ops.writeNumber4 cw.cost
        let read = reader {
            let! discriminator = Byte.read
            match discriminator with
            | SerializedPKWitness ->
                let! publicKey = readBytes Crypto.SerializedPublicKeyLength
                let! signature = readBytes Crypto.SerializedSignatureLength
                return PKWitness (publicKey, Signature signature)
            | SerializedContractWitness ->
                let! cHash = Hash.read
                let! command = readString
                let! dataLength = readNumber4
                let! data = readBytes (int dataLength)
                let! returnAddressIndex = Option.read readNumber4
                let! beginInputs = readNumber4
                let! beginOutputs = readNumber4
                let! inputsLength = readNumber4
                let! outputsLength = readNumber4
                let! cost = readNumber4
                return ContractWitness {
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
                yield! fail
        }
    
    module Lock =
        [<Literal>]
        let private SerializedPK = 1uy
        [<Literal>]
        let private SerializedContract = 2uy
        [<Literal>]
        let private SerializedCoinbase = 3uy
        [<Literal>]
        let private SerializedFee = 4uy
        [<Literal>]
        let private SerializedActivationSacrifice = 5uy
        [<Literal>]
        let private SerializedDestroy = 6uy
    
        let write ops = function
            | PK hash ->
                ops.writeByte SerializedPK
                >> ops.writeHash hash
            | Contract hash ->
                ops.writeByte SerializedContract
                >> ops.writeHash hash
            | Coinbase (blockNumber, pkHash) ->
                ops.writeByte SerializedCoinbase
                >> ops.writeNumber4 blockNumber
                >> ops.writeHash pkHash
            | Fee ->
                ops.writeByte SerializedFee
            | ActivationSacrifice ->
                ops.writeByte SerializedActivationSacrifice
            | Destroy ->
                ops.writeByte SerializedDestroy
        let read = reader {
            let! discriminator = Byte.read
            match discriminator with
            | SerializedPK ->
                let! hash = Hash.read
                return Lock.PK (Hash.Hash hash)
            | SerializedContract ->
                let! hash = Hash.read
                return Lock.Contract (Hash.Hash hash)
            | SerializedCoinbase ->
                let! blockNumber = readNumber4
                let! pkHash = Hash.read
                return Lock.Coinbase (blockNumber, Hash.Hash pkHash)
            | SerializedFee ->
                return Lock.Fee
            | SerializedActivationSacrifice ->
                return Lock.ActivationSacrifice
            | SerializedDestroy ->
                return Lock.Destroy
            | _ ->
                yield! fail
        }
    
    module Output =
        let write ops = fun { lock = lock; spend = spend } ->
            Lock.write ops lock
            >> Spend.write ops spend
        let read = reader {
            let! lock = Lock.read
            let! spend = Spend.read
            return { lock = lock; spend = spend }
        }
    
    module Contract =
        let write ops =
            Option.write ops (fun (code, hints) ->
                ops.writeLongString code
                >> ops.writeLongString hints)
        let read = reader {
            let readContract = reader {
                let! code = readLongString
                let! hints = readLongString
                return code, hints
            }
            let! contract = Option.read readContract
            return contract
        }
    
    module Nonce =
        let write ops = fun (nonce1, nonce2) ->
            ops.writeNumber8 nonce1
            >> ops.writeNumber8 nonce2
        let read = reader {
            let! nonce1 = readNumber8
            let! nonce2 = readNumber8
            return nonce1,nonce2
        }
    
    module Transaction =
        let write mode ops = fun tx ->
            List.write ops Input.write tx.inputs
            >> List.write ops Output.write tx.outputs
            >> Contract.write ops tx.contract
            >> 
            match mode with
            | Full -> List.write ops Witness.write tx.witnesses
            | WithoutWitness -> id
        let read mode = reader {
            let! inputs = List.read Input.read
            let! outputs = List.read Output.read
            let! contract = Contract.read
            let! witnesses = reader {
                match mode with
                | Full -> yield! List.read Witness.read
                | WithoutWitness -> return []
            }
            return { inputs = inputs; witnesses = witnesses; outputs = outputs; contract = contract }
        }
    
    module Header =
        let write ops = fun header ->
            ops.writeNumber4 header.version
            >> ops.writeHash header.parent
            >> ops.writeNumber4 header.blockNumber
            >> ops.writeHash header.commitments
            >> ops.writeNumber8 header.timestamp
            >> ops.writeNumber4 header.difficulty
            >> Nonce.write ops header.nonce
        let read = reader {
            let! version = readNumber4
            let! parent = Hash.read
            let! blockNumber = readNumber4
            let! commitments = Hash.read
            let! timestamp = readNumber8
            let! difficulty = readNumber4
            let! nonce = Nonce.read
            return {
                version = version
                parent = Hash.Hash parent
                blockNumber = blockNumber
                commitments = Hash.Hash commitments
                timestamp = timestamp
                difficulty = difficulty
                nonce = nonce
            }
        }
    
    module Block =
        let write ops bk =
            Header.write ops bk.header
            >> List.write ops (fun ops hash -> ops.writeHash hash) ([ bk.txMerkleRoot; bk.witnessMerkleRoot; bk.activeContractSetMerkleRoot ] @ bk.commitments)
            >> List.write ops (fun ops tx -> Transaction.write Full ops tx) bk.transactions
        let read = reader {
            let! header = Header.read
            let! commitments = List.read Hash.read
            let! transactions = List.read (Transaction.read Full)
            
            if List.length commitments < 3 then
                yield! fail
            else return {
                header = header
                txMerkleRoot = Hash.Hash commitments.[0]
                witnessMerkleRoot = Hash.Hash commitments.[1]
                activeContractSetMerkleRoot = Hash.Hash commitments.[2]
                commitments = List.map Hash.Hash commitments.[3 .. List.length commitments - 1]
                transactions = transactions
            }
        }

open Serialization

module Transaction =
    let serialize mode tx =
        Transaction.write mode counters tx 0
        |> create 
        |> Transaction.write mode serializers tx
        |> getBuffer
    let deserialize mode bytes =
        Stream (bytes, 0)
        |> run (Transaction.read mode)

module Header =
    let serialize header =
        create SerializedHeaderSize
        |> Header.write serializers header
        |> getBuffer
    let deserialize bytes =
        if Array.length bytes <> SerializedHeaderSize then
            None
        else
            Stream (bytes, 0)
            |> run Header.read

module Block =
    let serialize bk =
        Block.write counters bk 0
        |> create
        |> Block.write serializers bk
        |> getBuffer
    let deserialize bytes =
        let readBk = reader {
            let! header = Header.read
            let! commitmentsList = List.read Hash.read
            let! transactions = List.read (Transaction.read Full)
            
            return {
                header = header
                txMerkleRoot = Hash.Hash commitmentsList.[0]
                witnessMerkleRoot = Hash.Hash commitmentsList.[1]
                activeContractSetMerkleRoot = Hash.Hash commitmentsList.[2]
                commitments = List.map Hash.Hash commitmentsList.[3 .. List.length commitmentsList - 1]
                transactions = transactions
            }
        }
        
        Stream (bytes, 0)
        |> run readBk