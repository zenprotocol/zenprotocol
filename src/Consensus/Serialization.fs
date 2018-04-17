module Consensus.Serialization

open System
open Crypto
open Types
open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

module ZData = Zen.Types.Data

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

    let ofOption opt stream = opt,stream

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

    module Signature =

        let write ops signature =
            ops.writeBytes (Signature.serialize signature) SerializedSignatureLength

        let read = reader {
            let! bytes = readBytes SerializedSignatureLength
            let! signature = Signature.deserialize bytes |> ofOption

            return signature
        }

    module PublicKey =
        let write ops publicKey =
            ops.writeBytes (PublicKey.serialize publicKey) SerializedPublicKeyLength

        let read = reader {
            let! bytes = readBytes SerializedPublicKeyLength
            let! publicKey = PublicKey.deserialize bytes |> ofOption

            return publicKey
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
        let private SerializedExtensionSacrifice = 6uy
        [<Literal>]
        let private SerializedDestroy = 7uy

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
            | ExtensionSacrifice cHash ->
                ops.writeByte SerializedExtensionSacrifice
                >> ops.writeHash cHash
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
            | SerializedExtensionSacrifice ->
                let! cHash = Hash.read
                return Lock.ExtensionSacrifice (Hash.Hash cHash)
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
        let private SignatureData = 17uy
        [<Literal>]
        let private PublicKeyData = 18uy

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

        module Signature =
            let write ops signature = Signature.write ops (Signature signature)

            let read = reader {
                let! (Signature signature) = Signature.read

                return signature
            }

        module PublicKey =
            let write ops publicKey = PublicKey.write ops (PublicKey publicKey)

            let read = reader {
                 let! (PublicKey publicKey) = PublicKey.read

                 return publicKey
            }

        let rec write ops = function
            | ZData.I64 i ->
                ops.writeByte I64Data
                >> Int64.write ops i
            | ZData.I64Array (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte I64ArrayData
                >> Array.write ops Int64.write arr
            | ZData.Byte b ->
                ops.writeByte ByteData
                >> ops.writeByte b
            | ZData.ByteArray (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte ByteArrayData
                >> Array.write ops (fun ops -> ops.writeByte) arr
            | ZData.U32 i ->
                ops.writeByte U32Data
                >> ops.writeNumber4 i
            | ZData.U32Array (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte U32ArrayData
                >> Array.write ops (fun ops -> ops.writeNumber4) arr
            | ZData.U64 i ->
                ops.writeByte U64Data
                >> ops.writeNumber8 i
            | ZData.U64Array (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte U64ArrayData
                >> Array.write ops (fun ops -> ops.writeNumber8) arr
            | ZData.String s ->
                ops.writeByte StringData
                >> String.write ops s
            | ZData.StringArray (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte StringArrayData
                >> Array.write ops String.write arr
            | ZData.Hash hash ->
                ops.writeByte HashData
                >> Hash.write ops hash
            | ZData.HashArray (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte HashArrayData
                >> Array.write ops Hash.write arr
            | ZData.Lock l ->
                ops.writeByte LockData
                >> Lock.write ops l
            | ZData.LockArray (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte LockArrayData
                >> Array.write ops Lock.write arr
            | ZData.Tuple (a, b) ->
                ops.writeByte TupleData
                >> write ops a
                >> write ops b
            | ZData.Signature signature ->
                ops.writeByte SignatureData
                >> Signature.write ops signature
            | ZData.PublicKey publicKey ->
                ops.writeByte PublicKeyData
                >> PublicKey.write ops publicKey
            | ZData.Dict (ZData.DataDict (map, len)) ->
                let entries = Map.toList map
                ops.writeByte DictData
                >> ops.writeNumber4 len
                >> List.writeBody ops String.write (entries |> List.map fst)
                >> List.writeBody ops write (entries |> List.map snd)

        let rec read = reader {
            let! discriminator = Byte.read
            match discriminator with
            | I64Data ->
                let! i = Int64.read
                return ZData.I64 i
            | I64ArrayData ->
                let! arr = Array.readDtuple Int64.read
                return ZData.I64Array arr
            | ByteData ->
                let! byte = Byte.read
                return ZData.Byte byte
            | ByteArrayData ->
                let! arr = Array.readDtuple Byte.read
                return ZData.ByteArray arr
            | U32Data ->
                let! i = readNumber4
                return ZData.U32 i
            | U32ArrayData ->
                let! arr = Array.readDtuple readNumber4
                return ZData.U32Array arr
            | U64Data ->
                let! i = readNumber8
                return ZData.U64 i
            | U64ArrayData ->
                let! arr = Array.readDtuple readNumber8
                return ZData.U64Array arr
            | StringData ->
                let! s = String.read
                return ZData.String s
            | StringArrayData ->
                let! arr = Array.readDtuple String.read
                return ZData.StringArray arr
            | HashData ->
                let! hash = Hash.read
                return ZData.Hash hash
            | HashArrayData ->
                let! arr = Array.readDtuple Hash.read
                return ZData.HashArray arr
            | LockData ->
                let! lock = Lock.read
                return ZData.Lock lock
            | LockArrayData ->
                let! arr = Array.readDtuple Lock.read
                return ZData.LockArray arr
            | SignatureData ->
                let! signature = Signature.read
                return ZData.Signature signature
            | PublicKeyData ->
                let! publicKey = PublicKey.read
                return ZData.PublicKey publicKey
            | TupleData ->
                let! a = read
                let! b = read
                return ZData.Tuple (a,b)
            | DictData ->
                let! len = readNumber4
                let! keys = List.readBody String.read len
                let! values = List.readBody read len
                let map = List.zip keys values |> Map.ofSeq
                return ZData.Dict (ZData.DataDict (map, len))
            | _ ->
                yield! fail
        }

    module Witness =

        [<Literal>]
        let private SerializedPKWitness = 1uy
        [<Literal>]
        let private SerializedContractWitness = 2uy

        let write ops = function
            | PKWitness (publicKey, signature) ->
                ops.writeByte SerializedPKWitness
                >> PublicKey.write ops publicKey
                >> Signature.write ops signature
            | ContractWitness cw ->
                ops.writeByte SerializedContractWitness
                >> ops.writeHash cw.cHash
                >> ops.writeString cw.command
                >> Option.write ops (Data.write ops) cw.data
                >> ops.writeNumber4 cw.beginInputs
                >> ops.writeNumber4 cw.beginOutputs
                >> ops.writeNumber4 cw.inputsLength
                >> ops.writeNumber4 cw.outputsLength
                >> Option.write ops (fun sign ->
                    PublicKey.write ops (fst sign)
                    >> Signature.write ops (snd sign)
                  ) cw.signature
                >> ops.writeNumber4 cw.cost
        let read = reader {
            let! discriminator = Byte.read
            match discriminator with
            | SerializedPKWitness ->
                let! publicKey = PublicKey.read
                let! signature = Signature.read

                return PKWitness (publicKey, signature)
            | SerializedContractWitness ->
                let! cHash = Hash.read
                let! command = readString
                let! data = Option.read Data.read
                let! beginInputs = readNumber4
                let! beginOutputs = readNumber4
                let! inputsLength = readNumber4
                let! outputsLength = readNumber4
                let! signature = Option.read (reader {
                    let! publicKey = PublicKey.read
                    let! signature = Signature.read

                    return publicKey, signature
                })
                let! cost = readNumber4
                return ContractWitness {
                    cHash = Hash.Hash cHash
                    command = command
                    data = data
                    beginInputs = beginInputs
                    beginOutputs = beginOutputs
                    inputsLength = inputsLength
                    outputsLength = outputsLength
                    signature = signature
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
            Option.write ops (fun contract ->
                ops.writeLongString contract.code
                >> ops.writeLongString contract.hints
                >> ops.writeNumber4 contract.rlimit
                >> ops.writeNumber4 contract.queries)
        let read = reader {
            let readContract = reader {
                let! code = readLongString
                let! hints = readLongString
                let! rlimit = readNumber4
                let! queries = readNumber4
                return {
                    code = code
                    hints = hints
                    rlimit = rlimit
                    queries = queries
                }
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