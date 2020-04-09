module Blockchain.Serialization

open Consensus
open Serialization
open Serialization
open BlockState
open ExtendedBlockHeader
open Infrastructure
open UtxoSet

module ContractKey =
    let size (contract: ActiveContractSet.ContractKey) =
        ContractId.size contract.contractId + 4 + String.size contract.code

    let write stream (contract: ActiveContractSet.ContractKey) =
        ContractId.write stream contract.contractId
        stream.writeNumber4 contract.expiry
        String.write stream contract.code

    let read stream =
       let contractId = ContractId.read stream
       let expiry = stream.readNumber4 ()
       let code = String.read stream

       ({
            contractId=contractId
            expiry=expiry
            code=code
       }:ActiveContractSet.ContractKey)

    let serialize = serialize size write
    let deserialize = deserialize read

module BlockState =

    let private size blockState =
        4 +
            Seq.size (fun _ -> 8) blockState.ema.delayed +
            Seq.size (fun (contractId, contractOption) ->
                ContractId.size contractId +
                    Option.size ContractKey.size contractOption) blockState.activeContractSetUndoData +
            Seq.size (fun (contractId, dataOption) ->
                ContractId.size contractId +
                    Option.size Data.size dataOption) blockState.contractStatesUndoData +
            (if CGP.isEmpty blockState.cgp then 0 else CGP.size blockState.cgp)

    let private write (stream:Stream) blockState =
        stream.writeNumber4 blockState.ema.difficulty
        Seq.write (fun stream -> stream.writeNumber8) stream blockState.ema.delayed
        Seq.write (fun stream (contractId, contractOption) ->
            ContractId.write stream contractId
            Option.write stream ContractKey.write contractOption) stream blockState.activeContractSetUndoData
        Seq.write (fun stream (contractId, dataOption) ->
            ContractId.write stream contractId
            Option.write stream Data.write dataOption) stream blockState.contractStatesUndoData
        if not <| CGP.isEmpty blockState.cgp then
            CGP.write stream blockState.cgp

    let private read (stream:Stream) =
        let difficulty = stream.readNumber4 ()
        let delayed = List.read (fun stream -> stream.readNumber8 ()) stream
        let activeContractSetUndoData = List.read (fun stream ->
                                            let contractId = ContractId.read stream
                                            let contractOption = Option.read ContractKey.read stream

                                            contractId,contractOption
                                        ) stream

        let contractStatesUndoData = List.read (fun stream ->
                                            let contractId = ContractId.read stream
                                            let dataOption = Option.read Data.read stream
                                            contractId, dataOption
                                        ) stream

        let conditionalRead (stream:Stream) defaultValue readerFn =
            if stream.Offset = stream.Buffer.Length then
                defaultValue
            else
                readerFn stream

        let cgp = conditionalRead stream CGP.empty CGP.read

        {
            ema = { difficulty = difficulty; delayed = delayed }
            activeContractSetUndoData = activeContractSetUndoData
            contractStatesUndoData = contractStatesUndoData
            cgp = cgp
        }

    let serialize = serialize size write
    let deserialize = deserialize read

module private BlockStatus =
    [<Literal>]
    let private SerializedOrphan = 1uy
    [<Literal>]
    let private SerializedConnected = 2uy
    [<Literal>]
    let private SerializedMainChain = 3uy
    [<Literal>]
    let private SerializedInvalid = 4uy

    let size = 1

    let write stream = function
        | Orphan -> Byte.write stream SerializedOrphan
        | Connected -> Byte.write stream SerializedConnected
        | MainChain -> Byte.write stream SerializedMainChain
        | Invalid -> Byte.write stream SerializedInvalid

    let read stream =
        let discriminator = Byte.read stream
        match discriminator with
        | SerializedOrphan -> Orphan
        | SerializedConnected -> Connected
        | SerializedMainChain -> MainChain
        | SerializedInvalid -> Invalid
        | _ -> raise SerializationException

module BigInt =
    let size = BigInteger.toBytes32 >> Bytes.size

    let write stream =
        BigInteger.toBytes32
        >> Bytes.write stream

    let read stream =
        let bytes = Bytes.read stream
        BigInteger.fromBytes32 bytes

module ExtendedBlockHeader =
    let private commitments bk = bk.commitments

    let size extendedBlockHeader =
        Hash.size
        + Header.size
        + BlockStatus.size
        + Option.size BigInt.size extendedBlockHeader.chainWork
        + Hash.size
        + Hash.size
        + Hash.size
        + Seq.size (fun _ -> Hash.size) (commitments extendedBlockHeader)

    let write stream extendedBlockHeader =
        Hash.write stream extendedBlockHeader.hash
        Header.write stream extendedBlockHeader.header
        BlockStatus.write stream extendedBlockHeader.status
        Option.write stream BigInt.write extendedBlockHeader.chainWork
        Hash.write stream extendedBlockHeader.txMerkleRoot
        Hash.write stream extendedBlockHeader.witnessMerkleRoot
        Hash.write stream extendedBlockHeader.activeContractSetMerkleRoot
        Seq.write Hash.write stream (commitments extendedBlockHeader)

    let read stream =
        let hash = Hash.read stream
        let header = Header.read stream
        let status = BlockStatus.read stream
        let chainWork = Option.read BigInt.read stream
        let txMerkleRoot = Hash.read stream
        let witnessMerkleRoot = Hash.read stream
        let activeContractSetMerkleRoot = Hash.read stream
        let commitments = List.read Hash.read stream

        {
            hash = hash
            header = header
            status = status
            chainWork = chainWork
            txMerkleRoot = txMerkleRoot
            witnessMerkleRoot = witnessMerkleRoot
            activeContractSetMerkleRoot = activeContractSetMerkleRoot
            commitments = commitments
        }

    let serialize = serialize size write
    let deserialize = deserialize read

module Outpoint =
    let serialize = serialize Outpoint.size Outpoint.write
    let deserialize = deserialize Outpoint.read

module OutputStatus =
    [<Literal>]
    let private SerializedNoOutput = 1uy
    [<Literal>]
    let private SerializedSpent = 2uy
    [<Literal>]
    let private SerializedUnspent = 3uy

    let size = function
        | NoOutput ->
           Byte.size
        | Spent output ->
           Byte.size
           + Output.size output
        | Unspent output ->
           Byte.size
           + Output.size output

    let write stream = function
        | NoOutput ->
            Byte.write stream SerializedNoOutput
        | Spent output ->
            Byte.write stream SerializedSpent
            Output.write stream output
        | Unspent output ->
            Byte.write stream SerializedUnspent
            Output.write stream output

    let read stream =
        let discriminator = Byte.read stream
        match discriminator with
        | SerializedNoOutput -> NoOutput
        | SerializedSpent ->
            let output = Output.read stream
            Spent output
        | SerializedUnspent ->
            let output = Output.read stream
            Unspent output
        | _ -> raise SerializationException

    let serialize = serialize size write
    let deserialize = deserialize read

module PointedOutput =
    let size (outpoint, output) = Outpoint.size outpoint + Output.size output

    let write stream (outpoint, output) =
        Outpoint.write stream outpoint
        Output.write stream output

    let read stream =
        let outpoint = Outpoint.read stream
        let output = Output.read stream
        outpoint, output

    let serialize = serialize size write
    let deserialize = deserialize read

module Version =
    let size = 4

    let private write (stream:Stream) version = uint32 version |> stream.writeNumber4
    let private read (stream:Stream) = stream.readNumber4 () |> int32

    let serialize = serialize (fun _ -> 4) write
    let deserialize = deserialize read

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
    let serialize = serialize ContractId.size ContractId.write
    let deserialize = deserialize ContractId.read

open Blockchain.Tally
open Tally.Types


module Tally =
    let size = fun (tally:Tally.T) ->
        Map.size (fun (_, votes) -> 
            Byte.size + 
            Amount.size votes) tally.coinbaseRatio +
        Map.size (fun ((recipient, spend), votes) -> 
            Payout.Recipient.size recipient + 
            List.size Spend.size spend + 
            Amount.size votes) tally.payout

    let write (stream:Stream) = fun (tally:Tally.T) ->
        Map.write (fun stream (allocation, votes)->
            Byte.write stream allocation
            Amount.write stream votes) stream (tally.coinbaseRatio |> Tally.mapMapKeys Tally.getRatio)
        Map.write (fun stream ((recipient, spend), votes)->
            Payout.Recipient.write stream recipient
            List.write Spend.write stream spend
            Amount.write stream votes) stream tally.payout

    let read (stream:Stream) =
        {
            coinbaseRatio = Map.read (fun stream -> Byte.read stream |> Tally.CoinbaseRatio, Amount.read stream) stream
            payout = Map.read (fun stream -> Payout.read stream, Amount.read stream) stream
        } : Tally.T
        
    let serialize = Serialization.serialize size write
    let deserialize = Serialization.deserialize read

module Fund =
    let size balance =
        Map.size (fun (asset, amount) -> 
            Asset.size asset + 
            Amount.size amount) balance
        
    let write (stream:Stream) = fun (balance:Fund.T) ->
        Map.write (fun stream (asset, amount)->
                Asset.write stream asset
                Amount.write stream amount) stream balance
    let read (stream:Stream) =
        Map.read (fun stream -> Asset.read stream, Amount.read stream) stream
        
    let serialize = Serialization.serialize size write
    let deserialize = Serialization.deserialize read


module PKBalance =
    let size pkbalance =
        Map.size (fun (_, balance) ->
            Hash.size +
            Amount.size balance) pkbalance
    let write (stream:Stream) = fun (pkbalance:PKBalance) ->
        Map.write (fun stream (pk, balance)->
                Hash.write stream pk
                Amount.write stream balance) stream pkbalance
    let read (stream:Stream) =
        Map.read (fun stream -> Hash.read stream, Amount.read stream) stream

    let serialize = Serialization.serialize size write
    let deserialize = Serialization.deserialize read
    
module PKAllocation =
    let size pkAllocation =
        Map.size (fun _ ->
            PublicKey.size +
            Byte.size ) pkAllocation
    let write (stream:Stream) = fun (pkAllocation:PKAllocation) ->
        Map.write (fun stream (pk, allocation)->
                PublicKey.write stream pk
                Byte.write stream allocation) stream pkAllocation
    let read (stream:Stream) =
        Map.read (fun stream -> PublicKey.read stream, Byte.read stream) stream

    let serialize = Serialization.serialize size write
    let deserialize = Serialization.deserialize read
        
module PKPayout =
    let size pkPayout =
        Map.size (fun (_,payout) ->
            PublicKey.size +
            Payout.size payout ) pkPayout
    let write (stream:Stream) = fun (pkPayout:PKPayout) ->
        Map.write (fun stream (pk, allocation)->
                PublicKey.write stream pk
                Payout.write stream allocation) stream pkPayout
    let read (stream:Stream) =
        Map.read (fun stream -> PublicKey.read stream, Payout.read stream) stream

    let serialize = Serialization.serialize size write
    let deserialize = Serialization.deserialize read
    
module Winner =
    //TOOD: make allocation a byte instead of an option<Byte> (not urgent)
    let size = fun ({ allocation = allocation; payout = payout } : Types.Winner) ->
        Option.size (fun _ ->  Allocation.size) allocation +
        Option.size Payout.size payout

    let write stream = fun ({ allocation = allocation; payout = payout }: Types.Winner)  ->
        Option.write stream (fun stream allocation -> Allocation.write stream allocation) allocation
        Option.write stream (fun stream payout -> Payout.write stream payout) payout

    let read stream : Types.Winner =
        {
            allocation = Option.read Allocation.read stream
            payout = Option.read Payout.read stream
        }

    let serialize = serialize size write
    let deserialize = deserialize read
    
module Nominees =
    let size : Payout list -> int =
        List.size Payout.size
    
    let write stream =
        List.write Payout.write stream
        
    let read stream : Payout list =
        List.read Payout.read stream
        
    let serialize = serialize size write
    let deserialize = deserialize read
