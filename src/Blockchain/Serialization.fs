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
        >> Seq.write ops (fun ops contractState ->
            ContractId.write ops contractState.contractId
            >> ops.writeNumber4 contractState.expiry
            >> String.write ops contractState.code) blockState.activeContractSet
        >> Seq.write ops (fun ops (contractId, dataOption) ->
            ContractId.write ops contractId
            >> Option.write ops Data.write dataOption) blockState.contractStatesUndoData
        
    let private read = reader {
        let! difficulty = readNumber4
        let! delayed = List.read readNumber8
        let! activeContractSet = List.read <| reader {
            let! contractId = ContractId.read
            let! expiry = readNumber4
            let! code = String.read
            return {
                contractId=contractId
                expiry=expiry
                code=code
            }
        }
        
        let! contractStatesUndoData = List.read <| reader {
            let! contractId = ContractId.read
            let! dataOption = Option.read Data.read
            return contractId, dataOption
        }
        
        return {
            ema = { difficulty = difficulty; delayed = delayed }
            activeContractSet = activeContractSet
            contractStatesUndoData = contractStatesUndoData
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
        | Orphan -> Byte.write ops SerializedOrphan
        | Connected -> Byte.write ops SerializedConnected
        | MainChain -> Byte.write ops SerializedMainChain
        | Invalid -> Byte.write ops SerializedInvalid

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
        >> Bytes.write ops

    let read = reader {
        let! bytes = Bytes.read
        return BigInteger.fromBytes32 bytes
    }

module ExtendedBlockHeader =
    let write ops extendedBlockHeader =
        Hash.write ops extendedBlockHeader.hash
        >> Header.write ops extendedBlockHeader.header
        >> BlockStatus.write ops extendedBlockHeader.status
        >> Option.write ops BigInt.write extendedBlockHeader.chainWork
        >> Hash.write ops extendedBlockHeader.txMerkleRoot
        >> Hash.write ops extendedBlockHeader.witnessMerkleRoot
        >> Hash.write ops extendedBlockHeader.activeContractSetMerkleRoot
        >> Seq.write ops Hash.write extendedBlockHeader.commitments

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

module OutputStatus =
    [<Literal>]
    let private SerializedNoOutput = 1uy
    [<Literal>]
    let private SerializedSpent = 2uy
    [<Literal>]
    let private SerializedUnspent = 3uy

    let write ops = function
        | NoOutput ->
            Byte.write ops SerializedNoOutput
        | Spent output ->
            Byte.write ops SerializedSpent
            >> Output.write ops output
        | Unspent output ->
            Byte.write ops SerializedUnspent
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
        
module ContractId =
    let serialize outputStatus =
        ContractId.write counters outputStatus 0ul
        |> int32
        |> create
        |> ContractId.write serializers outputStatus
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run ContractId.read