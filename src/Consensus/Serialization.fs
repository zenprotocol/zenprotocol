module Consensus.Serialization

open System
open Crypto
open Hash
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

module Serialization =
    module Hash =
        let write hash = writeBytes (Hash.bytes hash) Hash.Length
        let readBytes = readBytes Hash.Length
        let read = reader {
            let! bytes = readBytes
            return Hash bytes
        }
        
    module Byte =
        let write byte = writeNumber1 byte
        let read = readNumber1

    type Operations<'a> = {
        writeHash: Hash -> 'a -> 'a
        writeByte: Byte -> 'a -> 'a
        writeBytes: Byte[] -> int32 -> 'a -> 'a
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

    let counters: Operations<uint32> = {
        writeHash = fun _ l -> l + (uint32 <| Hash.Length)
        writeByte = fun _ l -> l + 1u
        writeBytes = fun _ len l -> l + uint32 len
        writeString = fun str l -> l + 1u + (uint32 <| String.length str)
        writeLongString = fun str l -> l + 4u + (uint32 <| String.length str)
        writeNumber4 = fun _ l -> l + 4u
        writeNumber8 = fun _ l -> l + 8u
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
        let read readerFn = reader {
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

    module VarInt =
        // https://github.com/bitcoin/bitcoin/blob/v0.13.2/src/serialize.h#L307L372

        let write ops (x:uint32) =
            let tmp = Array.zeroCreate 5

            let rec loop n len =
                tmp.[len] <- (byte (n &&& 0x7Ful)) ||| (if len <> 0 then 0x80uy else 0x00uy)

                if n <= 0x7Ful then
                    len
                else
                    loop ((n >>> 7) - 1ul) (len + 1)

            let len = loop x 0
            let bytes = tmp.[0..len] |> Array.rev

            ops.writeBytes bytes (len + 1)

        let read =
            let rec loop n = reader {
                let! data = readNumber1

                let n = ((uint32 n) <<< 7) ||| ((uint32 data) &&& 0x7Ful)

                if data &&& 0x80uy <> 0uy then
                    yield! (loop (n + 1ul))
                else
                    yield n
            }

            loop 0ul

    module List =
        let writeBody ops writerFn list =
            let write list writerFn stream = //TODO: use fold?
                let mutable stream = stream //TODO: this is not 'stream' if ops are Counters
                for item in list do
                    stream <- writerFn ops item stream
                stream
            write list writerFn
        let write ops writerFn list =
            VarInt.write ops (List.length list |> uint32)
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
            let! length = VarInt.read
            let! list = readBody readerFn length
            return List.ofSeq list
        }
        
    module Map =
        let write ops writerFn =
            Map.toList
            >> List.write ops writerFn
        let read readerFn = reader {
            let! list = List.read readerFn 
            return (List.toSeq >> Map.ofSeq) list
        }

    module Asset =
        let write ops = fun (cHash, token) ->
            ops.writeHash cHash
            >> ops.writeHash token

        let read = reader {
            let! cHash = Hash.read
            let! token = Hash.read
            return cHash, token
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

    module Outpoint =
        let write ops = fun { txHash = txHash; index = index } ->
            ops.writeHash txHash
            >> ops.writeNumber4 index
        let read = reader {
            let! txHash = Hash.read
            let! index = readNumber4
            return { txHash = txHash; index = index }
        }

    module Input =
        [<Literal>]
        let private SerializedOutpoint = 1uy
        [<Literal>]
        let private SerializedMint = 2uy

        let write ops = function
            | Outpoint outpoint ->
                ops.writeByte SerializedOutpoint
                >> Outpoint.write ops outpoint
            | Mint spend ->
                ops.writeByte SerializedMint
                >> Spend.write ops spend
        let read = reader {
            let! discriminator = Byte.read
            match discriminator with
            | SerializedOutpoint ->
                let! outpoint = Outpoint.read 
                return Outpoint outpoint
            | SerializedMint ->
                let! spend = Spend.read
                return Mint spend
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
        let private PKIdentifier = 1u
        [<Literal>]
        let private ContractIdentifier = 2u
        [<Literal>]
        let private CoinbaseIdentifier = 3u
        [<Literal>]
        let private FeeIdentifier = 4u
        [<Literal>]
        let private ActivationSacrificeIdentifier = 5u
        [<Literal>]
        let private ExtensionSacrificeIdentifier = 6u
        [<Literal>]
        let private DestroyIdentifier = 7u

        let private writerFn ops = function
            | PK hash
            | Contract hash
            | ExtensionSacrifice hash ->
                ops.writeHash hash
            | Fee
            | ActivationSacrifice
            | Destroy ->
                id
            | Coinbase (blockNumber, pkHash) ->
                ops.writeNumber4 blockNumber
                >> ops.writeHash pkHash
            | HighVLock (_, bytes) ->
                List.writeBody ops (fun ops -> ops.writeByte) (Array.toList bytes)

        let write ops = fun lock ->
            let identifier =
                match lock with
                | PK _ -> PKIdentifier
                | Contract _ -> ContractIdentifier
                | Coinbase _ -> CoinbaseIdentifier
                | Fee -> FeeIdentifier
                | ActivationSacrifice -> ActivationSacrificeIdentifier
                | ExtensionSacrifice _ -> ExtensionSacrificeIdentifier
                | Destroy -> DestroyIdentifier
                | HighVLock (identifier, _) -> identifier

            VarInt.write ops identifier
            >> VarInt.write ops (writerFn counters lock 0ul)
            >> writerFn ops lock

        let read = reader {
            let! identifier = VarInt.read
            let! count = VarInt.read
            let! value = reader {
                match identifier with
                | PKIdentifier ->
                    let! hash = Hash.read
                    return Lock.PK hash
                | ContractIdentifier ->
                    let! hash = Hash.read
                    return Lock.Contract hash
                | CoinbaseIdentifier ->
                    let! blockNumber = readNumber4
                    let! pkHash = Hash.read
                    return Lock.Coinbase (blockNumber, pkHash)
                | FeeIdentifier ->
                    return Lock.Fee
                | ActivationSacrificeIdentifier ->
                    return Lock.ActivationSacrifice
                | DestroyIdentifier ->
                    return Lock.Destroy
                | ExtensionSacrificeIdentifier ->
                    let! cHash = Hash.read
                    return Lock.ExtensionSacrifice cHash
                | _ ->
                    let! bytes = List.readBody Byte.read count
                    return HighVLock (identifier, Array.ofList bytes)
            }

            do! check (count = writerFn counters value 0ul)
            return value
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

        module ZHash =
            let write ops = Hash.Hash >> ops.writeHash
            let read = reader {
                let! hash = Hash.read
                return hash |> Hash.bytes |> ZData.Hash
            }
            
        module ZLock =
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
                >> ZHash.write ops hash
            | ZData.HashArray (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte HashArrayData
                >> Array.write ops ZHash.write arr
            | ZData.Lock l ->
                ops.writeByte LockData
                >> ZLock.write ops l
            | ZData.LockArray (Prims.Mkdtuple2 (_, arr)) ->
                ops.writeByte LockArrayData
                >> Array.write ops ZLock.write arr
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

            | ZData.Dict (ZData.DataDict (map, _)) ->
                Map.write ops (fun ops (key, data)-> 
                    String.write ops key
                    >> write ops data) map

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
                yield! ZHash.read
            | HashArrayData ->
                let! arr = Array.readDtuple Hash.readBytes
                return ZData.HashArray arr
            | LockData ->
                let! lock = ZLock.read
                return ZData.Lock lock
            | LockArrayData ->
                let! arr = Array.readDtuple ZLock.read
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
                let! map = Map.read <| reader {
                    let! key = String.read
                    let! data = read
                    return key, data
                }
                return ZData.Dict (ZData.DataDict (map, Map.count map |> uint32))
                
            | _ ->
                yield! fail
        }

    module Witness =
        [<Literal>]
        let private PKIdentifier = 1u
        [<Literal>]
        let private ContractIdentifier = 2u

        let private writerFn ops = function
            | PKWitness (publicKey, signature) ->
                PublicKey.write ops publicKey
                >> Signature.write ops signature
            | ContractWitness cw ->
                ops.writeHash cw.cHash
                >> ops.writeString cw.command
                >> Option.write ops (Data.write ops) cw.data
                >> VarInt.write ops cw.beginInputs
                >> VarInt.write ops cw.beginOutputs
                >> VarInt.write ops cw.inputsLength
                >> VarInt.write ops cw.outputsLength
                >> Option.write ops (fun (publicKey, signature) ->
                    PublicKey.write ops publicKey
                    >> Signature.write ops signature
                  ) cw.signature
                >> VarInt.write ops cw.cost
            | HighVWitness (_, bytes) ->
                List.writeBody ops (fun ops -> ops.writeByte) (Array.toList bytes)

        let write ops = fun witness ->
            let identifier =
                match witness with
                | PKWitness _ -> PKIdentifier
                | ContractWitness _ -> ContractIdentifier
                | HighVWitness (identifier, _) -> identifier

            VarInt.write ops identifier
            >> VarInt.write ops (writerFn counters witness 0ul)
            >> writerFn ops witness

        let read = reader {
            let! identifier = VarInt.read
            let! count = VarInt.read
            let! value = reader {
                match identifier with
                | PKIdentifier ->
                    let! publicKey = PublicKey.read
                    let! signature = Signature.read
                    return PKWitness (publicKey, signature)
                | ContractIdentifier ->
                    let! cHash = Hash.read
                    let! command = readString
                    let! data = Option.read Data.read
                    let! beginInputs = VarInt.read
                    let! beginOutputs = VarInt.read
                    let! inputsLength = VarInt.read
                    let! outputsLength = VarInt.read
                    let! signature = Option.read (reader {
                        let! publicKey = PublicKey.read
                        let! signature = Signature.read
                        return publicKey, signature
                    })
                    let! cost = VarInt.read
                    return ContractWitness {
                        cHash = cHash
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
                    let! bytes = List.readBody Byte.read count
                    return HighVWitness (identifier, Array.ofList bytes)
            }

            do! check (count = writerFn counters value 0ul)
            return value
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
        let writerFn ops = function
            | V0 contract ->
                ops.writeLongString contract.code
                >> ops.writeLongString contract.hints
                >> VarInt.write ops contract.rlimit
                >> VarInt.write ops contract.queries
            | HighV (_, bytes) ->
                List.writeBody ops (fun ops -> ops.writeByte) (Array.toList bytes)

        let write ops = fun contract ->
            let identifier =
                match contract with
                | V0 _ -> Version0
                | HighV (identifier, _) -> identifier

            VarInt.write ops identifier
            >> VarInt.write ops (writerFn counters contract 0ul)
            >> writerFn ops contract

        let read = reader {
            let! version = VarInt.read
            let! count = VarInt.read
            let! value = reader {
                match version with
                | Version0 ->
                    let! code = readLongString
                    let! hints = readLongString
                    let! rlimit = VarInt.read
                    let! queries = VarInt.read
                    return V0 {
                        code = code
                        hints = hints
                        rlimit = rlimit
                        queries = queries
                    }
                | _ ->
                    let! bytes = List.readBody Byte.read count
                    return Contract.HighV (version, Array.ofList bytes)
            }

            do! check (count = writerFn counters value 0ul)
            return value
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
        let write mode ops = fun (tx:Transaction) ->
            ops.writeNumber4 tx.version
            >> List.write ops Input.write tx.inputs
            >> List.write ops Output.write tx.outputs
            >> Option.write ops (Contract.write ops) tx.contract
            >> match mode with
               | Full -> List.write ops Witness.write tx.witnesses
               | WithoutWitness -> id
        let read mode = reader {
            let! version = readNumber4
            let! inputs = List.read Input.read
            let! outputs = List.read Output.read
            let! contract = Option.read Contract.read
            let! witnesses = reader {
                match mode with
                | Full -> yield! List.read Witness.read
                | WithoutWitness -> return []
            }
            return { version = version; inputs = inputs; witnesses = witnesses; outputs = outputs; contract = contract }
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
                parent = parent
                blockNumber = blockNumber
                commitments = commitments
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
                txMerkleRoot = commitments.[0]
                witnessMerkleRoot = commitments.[1]
                activeContractSetMerkleRoot = commitments.[2]
                commitments = commitments.[3 .. List.length commitments - 1]
                transactions = transactions
            }
        }

open Serialization

module Transaction =
    let serialize mode tx =
        Transaction.write mode counters tx 0ul
        |> int32
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
        Block.write counters bk 0ul
        |> int32
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
                txMerkleRoot = commitmentsList.[0]
                witnessMerkleRoot = commitmentsList.[1]
                activeContractSetMerkleRoot = commitmentsList.[2]
                commitments = commitmentsList.[3 .. List.length commitmentsList - 1]
                transactions = transactions
            }
        }

        Stream (bytes, 0)
        |> run readBk

module Data =
    let serialize data =
        Data.write counters data 0ul
        |> int32
        |> create
        |> Data.write serializers data
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run Data.read