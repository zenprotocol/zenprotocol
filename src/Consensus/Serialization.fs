module Consensus.Serialization

open System
open Crypto
open Types
open Zen.Types.Data
open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

#nowarn "40"   // Ignore recurssive objects warnings

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
        let writeBody ops writerFn list =
            let write list writerFn stream = //TODO: use fold?
                let mutable stream = stream //TODO: this is not 'stream' if ops are Counters
                for item in list do
                    stream <- writerFn ops item stream
                stream
            write list writerFn
        let write ops writerFn list =
            ops.writeNumber4 (List.length list |> uint32)
            >> writeBody ops writerFn list
        let readBody readerFn length = reader {
            let! list = reader {
                for _ in [1..int length] do
                    let! item = readerFn
                    return item
            }
            return List.ofSeq list
        }
        let read readerFn = reader {
            let! length = readNumber4
            let! list = readBody readerFn length
            return List.ofSeq list
        }
    
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

    module Data =
        [<Literal>]
        let private I64Data = 1uy
        [<Literal>]
        let private I64ArrayData = 2uy
        [<Literal>]
        let private ByteData = 3uy
        [<Literal>]
        let private ByteArrayData = 4uy
        [<Literal>]
        let private U32Data = 5uy
        [<Literal>]
        let private U32ArrayData = 6uy
        [<Literal>]
        let private U64Data = 7uy
        [<Literal>]
        let private U64ArrayData = 8uy
        [<Literal>]
        let private StringData = 9uy
        [<Literal>]
        let private StringArrayData = 10uy
        [<Literal>]
        let private HashData = 11uy
        [<Literal>]
        let private HashArrayData = 12uy
        [<Literal>]
        let private LockData = 13uy
        [<Literal>]
        let private LockArrayData = 14uy
        [<Literal>]
        let private TupleData = 15uy
        [<Literal>]
        let private DictData = 16uy
        [<Literal>]
        let private EmptyData = 17uy

        module Int64 = 
            let write ops = uint64 >> ops.writeNumber8 
            let read = reader {
                let! i = readNumber8
                return int64 i
            }
            
        module Array =
            let write obs fn = Array.toList >> List.write obs fn
            let readDtuple readerFn = reader {
                let! seq = List.read readerFn
                return Prims.Mkdtuple2 (int64 (Seq.length seq), Array.ofSeq seq)
            }
        
        module Hash = 
            let write ops = Consensus.Hash.Hash >> ops.writeHash

        module Lock = 
            let write ops = ZFStar.fstToFsLock >> Lock.write ops
            let read = reader {
                let! lock = Lock.read
                return ZFStar.fsToFstLock lock
            }

        module String = 
            let write ops = ZFStar.fstToFsString >> ops.writeString 
            let read = reader {
                let! s = readString
                return ZFStar.fsToFstString s
            }
            
        let rec write ops = function
            | I64 i ->
                ops.writeByte I64Data
                >> Int64.write ops i
            | I64Array (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte I64ArrayData
                >> Array.write ops Int64.write arr
            | Byte b ->
                ops.writeByte ByteData
                >> ops.writeByte b
            | ByteArray (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte ByteArrayData
                >> Array.write ops (fun ops -> ops.writeByte) arr
            | U32 i ->
                ops.writeByte U32Data
                >> ops.writeNumber4 i
            | U32Array (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte U32ArrayData
                >> Array.write ops (fun ops -> ops.writeNumber4) arr
            | U64 i ->
                ops.writeByte U64Data
                >> ops.writeNumber8 i
            | U64Array (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte U64ArrayData
                >> Array.write ops (fun ops -> ops.writeNumber8) arr
            | String s ->
                ops.writeByte StringData
                >> String.write ops s
            | StringArray (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte StringArrayData
                >> Array.write ops String.write arr
            | Hash hash ->
                ops.writeByte HashData
                >> Hash.write ops hash
            | HashArray (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte HashArrayData
                >> Array.write ops Hash.write arr
            | Lock l ->
                ops.writeByte LockData
                >> Lock.write ops l
            | LockArray (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte LockArrayData
                >> Array.write ops Lock.write arr
            | Tuple (a, b) ->
                ops.writeByte TupleData
                >> write ops a
                >> write ops b
            | Dict (DataDict (map, len)) ->
                let entries = Map.toList map
                ops.writeByte DictData
                >> ops.writeNumber4 len
                >> List.writeBody ops String.write (entries |> List.map fst)
                >> List.writeBody ops write (entries |> List.map snd)
            | Empty ->
                ops.writeByte EmptyData

        let rec read = reader {
            let! discriminator = Byte.read
            match discriminator with
            | I64Data ->
                let! i = Int64.read
                return I64 i
            | I64ArrayData ->
                let! arr = Array.readDtuple Int64.read
                return I64Array arr
            | ByteData ->
                let! byte = Byte.read
                return Byte byte
            | ByteArrayData ->
                let! arr = Array.readDtuple Byte.read
                return ByteArray arr
            | U32Data ->
                let! i = readNumber4
                return U32 i
            | U32ArrayData ->
                let! arr = Array.readDtuple readNumber4
                return U32Array arr
            | U64Data ->
                let! i = readNumber8
                return U64 i
            | U64ArrayData ->
                let! arr = Array.readDtuple readNumber8
                return U64Array arr
            | StringData ->
                let! s = String.read
                return String s
            | StringArrayData ->
                let! arr = Array.readDtuple String.read
                return StringArray arr
            | HashData ->
                let! hash = Hash.read
                return Hash hash
            | HashArrayData ->
                let! arr = Array.readDtuple Hash.read
                return HashArray arr
            | LockData ->
                let! lock = Lock.read
                return Lock lock
            | LockArrayData ->
                let! arr = Array.readDtuple Lock.read
                return LockArray arr
            | TupleData ->
                let! a = read
                let! b = read
                return Tuple (a,b)
            | DictData ->
                let! len = readNumber4
                let! keys = List.readBody String.read len
                let! values = List.readBody read len
                let map = List.zip keys values |> Map.ofSeq
                return Dict (DataDict (map, len))
            | EmptyData ->
                return Empty
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
                ops.writeByte SerializedContractWitness
                >> ops.writeHash cw.cHash
                >> ops.writeString cw.command
                >> Data.write ops cw.data
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
                let! data = Data.read
                let! returnAddressIndex = Option.read readNumber4
                let! beginInputs = readNumber4
                let! beginOutputs = readNumber4
                let! inputsLength = readNumber4
                let! outputsLength = readNumber4
                let! cost = readNumber4
                return ContractWitness {
                    cHash = Hash.Hash cHash
                    command = command
                    data = data
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
            >> List.write ops (fun ops -> ops.writeHash) ([ bk.txMerkleRoot; bk.witnessMerkleRoot; bk.activeContractSetMerkleRoot ] @ bk.commitments)
            >> List.write ops (fun ops -> Transaction.write Full ops) bk.transactions
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

module Data =
    let serialize data =
        Data.write counters data 0
        |> create 
        |> Data.write serializers data
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run Data.read