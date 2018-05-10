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

//TODO: move into FsNetMQ
type ReaderBuilder() =
    member this.Bind (r,f) =
        fun (stream:Stream.T) ->
            let x, stream = r stream
            match x with
            | None -> None, stream
            | Some x -> f x stream
    member this.Return x = fun buffer -> Some x,buffer
    member this.Yield x = fun buffer -> Some x,buffer
    member this.YieldFrom (r:Reader<'a>) = fun (stream:Stream.T) -> r stream
    member this.For (seq,body) =
        fun (stream:Stream.T) ->
            let xs,stream = Seq.fold (fun (list, stream) i ->
                                match list with
                                | None -> None, stream
                                | Some list ->
                                    let x,stream = body i stream
                                    match x with
                                    | None -> None,stream
                                    | Some x -> Some (x :: list),stream
                            ) (Some [], stream) seq
            match xs with
            | None -> None, stream
            | Some xs -> Some (Seq.rev xs), stream
    member this.Delay f = fun stream -> f () stream

let reader = new ReaderBuilder()
///


[<Literal>]
let SerializedHeaderSize = 100

type TransactionSerializationMode =
    | Full
    | WithoutWitness

module Serialization =
    let getBytes (s:string) = System.Text.Encoding.ASCII.GetBytes s

    type Operations<'a> = {
        writeBytes: Byte[] -> int32 -> 'a -> 'a
        writeNumber1: uint8 -> 'a -> 'a
        writeNumber4: uint32 -> 'a -> 'a
        writeNumber8: uint64 -> 'a -> 'a
    }

    let serializers: Operations<Stream.T> = {
        writeBytes = writeBytes
        writeNumber1 = writeNumber1
        writeNumber4 = writeNumber4
        writeNumber8 = writeNumber8
    }

    let counters: Operations<uint32> = {
        writeBytes = fun _ len count -> count + (uint32 len)
        writeNumber1 = fun _ count -> count + 1u
        writeNumber4 = fun _ count -> count + 4u
        writeNumber8 = fun _ count -> count + 8u
    }

    let fail stream =
        None, stream

    let ofOption opt stream = opt,stream

    module Byte =
        let write ops byte = ops.writeNumber1 byte
        let read = readNumber1

    module Option =
        [<Literal>]
        let private None = 0uy
        [<Literal>]
        let private Some = 1uy

        let write ops writerFn = function
            | Option.Some value ->
                Byte.write ops Some
                >> writerFn value
            | Option.None ->
                Byte.write ops None
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
        let write ops = fun (x:uint32) ->
            // https://github.com/bitcoin/bitcoin/blob/v0.13.2/src/serialize.h#L307L372
            let tmp = Array.zeroCreate 5

            let rec loop n len =
                tmp.[len] <- (byte (n &&& 0x7Ful)) ||| (if len <> 0 then 0x80uy else 0x00uy)

                if n <= 0x7Ful then
                    len
                else
                    loop ((n >>> 7) - 1ul) (len + 1)

            let len = loop x 0
            let bytes = Array.rev tmp.[0..len]

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

    module Bytes =
        let write ops bytes =
            let len = Seq.length bytes
            VarInt.write ops (uint32 len)
            >> ops.writeBytes bytes len
        let read = reader {
            let! len = VarInt.read
            yield! readBytes (int len)
        }

    module FixedSizeBytes =
        let writeChecked ops bytes len =
            let actualLen = Seq.length bytes
            if len <> actualLen then failwith "byte array size mismatch"
            ops.writeBytes bytes len
        let write ops bytes =
            ops.writeBytes bytes (Seq.length bytes)
        let read len = readBytes len

    module Hash =
        let write ops hash = FixedSizeBytes.writeChecked ops (Hash.bytes hash) Hash.Length
        let read = reader {
            let! bytes = FixedSizeBytes.read Hash.Length
            return Hash bytes
        }

    module String =
        let write ops =
            getBytes
            >> Bytes.write ops
        let read = reader {
            let! bytes = Bytes.read
            return System.Text.Encoding.ASCII.GetString bytes
        }

    module Seq =
        let private writeBody ops writerFn =
            let write writerFn seq stream =
                //let mutable stream = stream //TODO: this is not 'stream' if ops are Counters
                //for item in seq do
                //    stream <- writerFn ops item stream
                //stream
                let aux stream item = writerFn ops item stream
                seq |> Seq.fold aux stream

            write writerFn
        let write ops writerFn seq =
            VarInt.write ops (Seq.length seq |> uint32)
            >> writeBody ops writerFn seq
        let read readerFn = reader {
            let! length = VarInt.read
            for _ in [1..int length] do
                yield! readerFn
        }

    module List =
        let write ops writerFn (ls: list<'B>) =
            Seq.write ops writerFn ls
        let read readerFn = reader {
            let! seq = Seq.read readerFn
            return List.ofSeq seq
        }

    module Array =
        let write ops writerFn (arr: array<'B>) =
            Seq.write ops writerFn arr
        let read readerFn = reader {
            let! seq = Seq.read readerFn
            return Array.ofSeq seq
        }

    module Map =
        let write ops writerFn =
            Map.toList
            >> Seq.write ops writerFn
        let read readerFn = reader {
            let! seq = Seq.read readerFn
            return Map.ofSeq seq
        }

    module ContractId =
        let write ops (ContractId (version,cHash)) =
            VarInt.write ops version
            >> Hash.write ops cHash
        let read = reader {
            let! version = VarInt.read
            let! cHash = Hash.read

            return ContractId (version, cHash)
        }

    module Asset =
        let write ops = fun (Asset (contractId,subType)) ->
            ContractId.write ops contractId
            >> Hash.write ops subType

        let read = reader {
            let! contractId = ContractId.read
            let! subType = Hash.read
            return Asset (contractId, subType)
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
            Hash.write ops txHash
            >> VarInt.write ops index
        let read = reader {
            let! txHash = Hash.read
            let! index = VarInt.read
            return { txHash = txHash; index = index }
        }

    module Input =
        [<Literal>]
        let private SerializedOutpoint = 1uy
        [<Literal>]
        let private SerializedMint = 2uy

        let write ops = function
            | Outpoint outpoint ->
                Byte.write ops SerializedOutpoint
                >> Outpoint.write ops outpoint
            | Mint spend ->
                Byte.write ops SerializedMint
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
            FixedSizeBytes.writeChecked ops (Signature.serialize signature) SerializedSignatureLength

        let read = reader {
            let! bytes = FixedSizeBytes.read SerializedSignatureLength
            let! signature = Signature.deserialize bytes |> ofOption

            return signature
        }

    module PublicKey =
        let write ops publicKey =
            FixedSizeBytes.writeChecked ops (PublicKey.serialize publicKey) SerializedPublicKeyLength

        let read = reader {
            let! bytes = FixedSizeBytes.read SerializedPublicKeyLength
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
            | PK hash -> Hash.write ops hash
            | Contract contractId -> ContractId.write ops contractId
            | ExtensionSacrifice contractId ->
                ContractId.write ops contractId
            | Fee
            | ActivationSacrifice
            | Destroy ->
                id
            | Coinbase (blockNumber, pkHash) ->
                ops.writeNumber4 blockNumber
                >> Hash.write ops pkHash
            | HighVLock (_, bytes) ->
                FixedSizeBytes.write ops bytes

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
                    let! contractId = ContractId.read
                    return Lock.Contract contractId
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
                    let! contractId = ContractId.read
                    return Lock.ExtensionSacrifice contractId
                | _ ->
                    let! bytes = FixedSizeBytes.read (int count)
                    return HighVLock (identifier, bytes)
            }

            do! check (count = writerFn counters value 0ul)
            return value
        }

    module Data =
        open Consensus
        open System.Collections.ObjectModel

        [<Literal>]
        let private I64Data = 1uy
        [<Literal>]
        let private ByteData = 2uy
<<<<<<< eeee0d17f609143c40f60071a5eb3ab9d64f30c8
        [<Literal>]
        let private ByteArrayData = 3uy
        [<Literal>]
        let private U32Data = 4uy
        [<Literal>]
        let private U64Data = 5uy
        [<Literal>]
        let private StringData = 6uy
        [<Literal>]
        let private HashData = 7uy
        [<Literal>]
        let private LockData = 8uy
        [<Literal>]
        let private SignatureData = 9uy
        [<Literal>]
        let private PublicKeyData = 10uy
        [<Literal>]
        let private CollectionArrayData = 11uy
        [<Literal>]
        let private CollectionDictData = 12uy
        [<Literal>]
=======
        [<Literal>]
        let private ByteArrayData = 3uy
        [<Literal>]
        let private U32Data = 4uy
        [<Literal>]
        let private U64Data = 5uy
        [<Literal>]
        let private StringData = 6uy
        [<Literal>]
        let private HashData = 7uy
        [<Literal>]
        let private LockData = 8uy
        [<Literal>]
        let private SignatureData = 9uy
        [<Literal>]
        let private PublicKeyData = 10uy
        [<Literal>]
        let private CollectionArrayData = 11uy
        [<Literal>]
        let private CollectionDictData = 12uy
        [<Literal>]
>>>>>>> updates to zulib
        let private CollectionListData = 13uy

        module Int64 =
            let write ops = uint64 >> ops.writeNumber8
            let read = reader {
                let! i = readNumber8
                return int64 i
            }

        module List =
            let write = Seq.write
            let read readerFn = reader {
                let! seq = Seq.read readerFn
                return List.ofSeq seq
            }

        module Array =
            let write = Seq.write
            let read readerFn = reader {
                let! seq = Seq.read readerFn
                return Array.ofSeq seq
            }

        module Hash =
            let write ops = Hash.Hash >> Hash.write ops
            let read = reader {
                let! hash = Hash.read
                return hash |> Hash.bytes
            }

        module Lock =
            let write ops = ZFStar.fstToFsLock >> Lock.write ops
            let read = reader {
                let! lock = Lock.read
                return ZFStar.fsToFstLock lock
            }

        module String =
            let write ops = ZFStar.fstToFsString >> String.write ops
            let read = reader {
                let! s = String.read
                return ZFStar.fsToFstString s
            }

        module Signature =
            let write ops = Signature >> Signature.write ops
            let read = reader {
                let! (Signature signature) = Signature.read
                return signature
            }

        module PublicKey =
            let write ops = PublicKey >> PublicKey.write ops
            let read = reader {
                 let! (PublicKey publicKey) = PublicKey.read
                 return publicKey
            }

        let rec write (ops: Operations<'a>)
                      : ZData.data -> 'a -> 'a = function
            | ZData.I64 i ->
                Byte.write ops I64Data
                >> Int64.write ops i
            | ZData.Byte b ->
                Byte.write ops ByteData
                >> Byte.write ops b
            | ZData.ByteArray arr ->
                Byte.write ops ByteArrayData
                >> Bytes.write ops arr
            | ZData.U32 i ->
                Byte.write ops U32Data
                >> ops.writeNumber4 i
            | ZData.U64 i ->
                Byte.write ops U64Data
                >> ops.writeNumber8 i
            | ZData.String s ->
                Byte.write ops StringData
                >> String.write ops s
            | ZData.Hash hash ->
                Byte.write ops HashData
                >> Hash.write ops hash
            | ZData.Lock l ->
                Byte.write ops LockData
                >> Lock.write ops l
            | ZData.Signature signature ->
                Byte.write ops SignatureData
                >> Signature.write ops signature
            | ZData.PublicKey publicKey ->
                Byte.write ops PublicKeyData
                >> PublicKey.write ops publicKey
            | ZData.Collection (ZData.Array arr) ->
                Byte.write ops CollectionArrayData
                >> Array.write ops write arr
            | ZData.Collection (ZData.Dict (map, _)) ->
                Byte.write ops CollectionDictData
                >> Map.write ops (fun ops (key, data)->
                    String.write ops key
                    >> write ops data) map
            | ZData.Collection (ZData.List l) ->
                Byte.write ops CollectionListData
                >> List.write ops write (ZFStar.fstToFsList l)

        let rec read = reader {
            let! discriminator = Byte.read
            match discriminator with
            | I64Data ->
                let! i = Int64.read
                return ZData.I64 i
            | ByteData ->
                let! byte = Byte.read
                return ZData.Byte byte
            | ByteArrayData ->
                let! arr = Array.read Byte.read
                return ZData.ByteArray arr
            | U32Data ->
                let! i = readNumber4
                return ZData.U32 i
            | U64Data ->
                let! i = readNumber8
                return ZData.U64 i
            | StringData ->
                let! s = String.read
                return ZData.String s
            | HashData ->
                let! hash = Hash.read
                return ZData.Hash hash
            | LockData ->
                let! lock = Lock.read
                return ZData.Lock lock
            | SignatureData ->
                let! signature = Signature.read
                return ZData.Signature signature
            | PublicKeyData ->
                let! publicKey = PublicKey.read
                return ZData.PublicKey publicKey
            | CollectionArrayData ->
                let! arr = Array.read read
                return ZData.Collection (ZData.Array arr)
            | CollectionDictData ->
                let! map = Map.read <| reader {
                    let! key = String.read
                    let! value = read
                    return key, value
                }
                return ZData.Collection (ZData.Dict (map, Map.count map |> uint32))
            | CollectionListData ->
                let! l = List.read read
                let l = ZFStar.fsToFstList (List.ofSeq l)
                return ZData.Collection (ZData.List l)
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
                ContractId.write ops cw.contractId
                >> String.write ops cw.command
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
                FixedSizeBytes.write ops bytes

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
                    let! contractId = ContractId.read
                    let! command = String.read
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
                        contractId = contractId
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
                    let! bytes = FixedSizeBytes.read (int count)
                    return HighVWitness (identifier, bytes)
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
        let private writerFn ops = function
            | V0 contract ->
                String.write ops contract.code
                >> String.write ops contract.hints
                >> VarInt.write ops contract.rlimit
                >> VarInt.write ops contract.queries
            | HighV (_, bytes) ->
                FixedSizeBytes.write ops bytes
        let write ops = fun contract ->
            let identifier =
                match contract with
                | V0 _ -> Version0
                | HighV (version, _) -> version

            VarInt.write ops identifier
            >> VarInt.write ops (writerFn counters contract 0ul)
            >> writerFn ops contract
        let read = reader {
            let! version = VarInt.read
            let! count = VarInt.read
            let! value = reader {
                match version with
                | Version0 ->
                    let! code = String.read
                    let! hints = String.read
                    let! rlimit = VarInt.read
                    let! queries = VarInt.read
                    return V0 {
                        code = code
                        hints = hints
                        rlimit = rlimit
                        queries = queries
                    }
                | _ ->
                    let! bytes = FixedSizeBytes.read (int count)
                    return Contract.HighV (version, bytes)
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
            >> Seq.write ops Input.write tx.inputs
            >> Seq.write ops Output.write tx.outputs
            >> Option.write ops (Contract.write ops) tx.contract
            >> match mode with
               | Full -> Seq.write ops Witness.write tx.witnesses
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
            >> Hash.write ops header.parent
            >> ops.writeNumber4 header.blockNumber
            >> Hash.write ops header.commitments
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
            >> Seq.write ops Hash.write ([ bk.txMerkleRoot; bk.witnessMerkleRoot; bk.activeContractSetMerkleRoot ] @ bk.commitments)
            >> Seq.write ops (Transaction.write Full) bk.transactions
        let read = reader {
            let! header = Header.read
            let! commitments = List.read Hash.read
            let! transactions = List.read (Transaction.read Full)

            if Seq.length commitments < 3 then
                yield! fail
            else return {
                header = header
                txMerkleRoot = commitments.[0]
                witnessMerkleRoot = commitments.[1]
                activeContractSetMerkleRoot = commitments.[2]
                commitments = commitments.[3 .. Seq.length commitments - 1]
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
                transactions = List.ofSeq transactions
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
