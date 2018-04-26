module Blockchain.Serialization

open Consensus.Serialization
open Consensus.Serialization.Serialization

open Consensus
open Hash
open BlockState
open ExtendedBlockHeader
open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader
open Infrastructure
open UtxoSet

module BlockState =
    let private write ops blockState = 
        ops.writeNumber4 blockState.ema.difficulty
        >> Seq.write ops (fun ops -> ops.writeNumber8) blockState.ema.delayed
        >> Seq.write ops (fun ops (cHash, blockNumber, code) -> 
            ops.writeHash cHash
            >> ops.writeNumber4 blockNumber
            >> ops.writeLongString code) blockState.activeContractSet

    let private read = reader {
        let! difficulty = readNumber4
        let! delayed = List.read readNumber8
        let! activeContractSet = List.read <| reader {
            let! cHash = Hash.read
            let! blockNumber = readNumber4
            let! code = readLongString
            return cHash, blockNumber, code
        }
        return {
            ema = { difficulty = difficulty; delayed = delayed }
            activeContractSet = activeContractSet
        }
    }

    let serialize blockState =
        write counters blockState 0ul
        |> int32
        |> FsNetMQ.Stream.create
        |> write serializers blockState
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run read
        |> function
        | Some value -> value
        | None -> failwith "could not deserialize blockState data"

module private BlockStatus =
    [<Literal>]
    let private SerializedOrphan = 1uy
    [<Literal>]
    let private SerializedConnected = 2uy
    [<Literal>]
    let private SerializedMainChain = 3uy
    [<Literal>]
    let private SerializedInvalid = 4uy

    let write ops = function
        | Orphan -> ops.writeByte SerializedOrphan
        | Connected -> ops.writeByte SerializedConnected
        | MainChain -> ops.writeByte SerializedMainChain
        | Invalid -> ops.writeByte SerializedInvalid
        
    let read = reader {
        let! discriminator = Byte.read
        match discriminator with 
        | SerializedOrphan -> return Orphan
        | SerializedConnected -> return Connected
        | SerializedMainChain -> return MainChain
        | SerializedInvalid -> return Invalid
        | _ -> yield! Serialization.fail
    }
    
module BigInt =
    let write ops =
        BigInteger.toBytes32 
        >> Seq.write ops (fun ops -> ops.writeByte)
    
    let read = reader {
        let! bytes = Array.read Byte.read
        return BigInteger.fromBytes32 bytes
    }
        
module ExtendedBlockHeader =
    let write ops extendedBlockHeader =
        ops.writeHash extendedBlockHeader.hash
        >> Header.write ops extendedBlockHeader.header
        >> BlockStatus.write ops extendedBlockHeader.status
        >> Option.write ops (BigInt.write ops) extendedBlockHeader.chainWork
        >> ops.writeHash extendedBlockHeader.txMerkleRoot
        >> ops.writeHash extendedBlockHeader.witnessMerkleRoot
        >> ops.writeHash extendedBlockHeader.activeContractSetMerkleRoot
        >> Seq.write ops (fun ops -> ops.writeHash) extendedBlockHeader.commitments

    let read = reader {
        let! hash = Hash.read
        let! header = Header.read
        let! status = BlockStatus.read
        let! chainWork = Option.read BigInt.read
        let! txMerkleRoot = Hash.read
        let! witnessMerkleRoot = Hash.read
        let! activeContractSetMerkleRoot = Hash.read
        let! commitments = List.read Hash.read
        
        return {
            hash = hash
            header = header
            status = status
            chainWork = chainWork
            txMerkleRoot = txMerkleRoot
            witnessMerkleRoot = witnessMerkleRoot
            activeContractSetMerkleRoot = activeContractSetMerkleRoot
            commitments = commitments
        }
    }

    let serialize extendedBlockHeader =
        write counters extendedBlockHeader 0ul
        |> int32
        |> create
        |> write serializers extendedBlockHeader
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run read
        |> function
        | Some value -> value
        | None -> failwith "could not deserialize ExtendedBlockHeader data"

module Outpoint =
    let serialize outpoint =
        Outpoint.write counters outpoint 0ul
        |> int32
        |> create
        |> Outpoint.write serializers outpoint
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run Outpoint.read
        |> function
        | Some value -> value
        | None -> failwith "could not deserialize Outpoint data"

module OutputStatus =
    [<Literal>]
    let private SerializedNoOutput = 1uy
    [<Literal>]
    let private SerializedSpent = 2uy
    [<Literal>]
    let private SerializedUnspent = 3uy

    let write ops = function
        | NoOutput ->
            ops.writeByte SerializedNoOutput
        | Spent output ->
            ops.writeByte SerializedSpent
            >> Output.write ops output
        | Unspent output ->
            ops.writeByte SerializedUnspent
            >> Output.write ops output
        
    let read = reader {
        let! discriminator = Byte.read
        match discriminator with 
        | SerializedNoOutput -> return NoOutput
        | SerializedSpent -> 
            let! output = Output.read
            return Spent output
        | SerializedUnspent -> 
            let! output = Output.read
            return Unspent output
        | _ -> yield! Serialization.fail
    }
    
    let serialize outputStatus =
        write counters outputStatus 0ul
        |> int32
        |> create
        |> write serializers outputStatus
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run read
        |> function
        | Some value -> value
        | None -> failwith "could not deserialize outputStatus data"

module PointedOutput =
    open Zen.Types.Extracted

    let write ops (outpoint, output) =
        Outpoint.write ops outpoint
        >> Output.write ops output
    
    let read = reader {
        let! outpoint = Outpoint.read
        let! output = Output.read
        return outpoint, output
    }
        
    let serialize outputStatus =
        write counters outputStatus 0ul
        |> int32
        |> create
        |> write serializers outputStatus
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run read
        |> function
        | Some value -> value
        | None -> failwith "could not deserialize pointedOutput data"

module Version =
    let private write ops = uint32 >> ops.writeNumber4
    let private read = reader {
        let! value = readNumber4
        return int32 value
    }
    
    let serialize version =
        write counters version 0ul
        |> int32
        |> create
        |> write serializers version
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run read
        |> function
        | Some value -> value
        | None -> failwith "could not deserialize version data"

module Hashes =
    let serialize hs =
        hs
        |> Seq.map Hash.bytes
        |> Array.concat
    
    let deserialize bytes =
        bytes
        |> Array.chunkBySize Hash.Length
        |> Array.toSeq
        |> Seq.map Hash.Hash