module Wallet.Serialization

open Consensus.Serialization
open Consensus.Serialization.Serialization

open Wallet.Account
open Consensus.Hash

open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

module private Status =
    [<Literal>]
    let private SerializedSpent = 0uy
    [<Literal>]
    let private SerializedUnspent = 1uy

    let write ops writerFn = function
        | Spent value ->
            ops.writeByte SerializedSpent
            >> writerFn value
        | Unspent value ->
            ops.writeByte SerializedUnspent
            >> writerFn value
    let read readerFn = reader {
        let! discriminator = Byte.read
        match discriminator with
        | SerializedSpent ->
            let! value = readerFn
            return Spent value
        | SerializedUnspent ->
            let! value = readerFn
            return Unspent value
        | _ ->
            yield! fail
    }

module private SpendStatus =
    let write ops =
        Status.write ops (Spend.write ops)
    let read =
        Status.read Spend.read   
    
module private OutputStatus =
    let write ops =
        Status.write ops (Output.write ops)
    let read =
        Status.read Output.read   
    
module private TxDelta =
    let write ops = fun txDelta ->
        ops.writeHash txDelta.txHash
        >> Seq.write ops SpendStatus.write txDelta.deltas
        >> Option.write ops ops.writeNumber4 txDelta.blockNumber
    let read = reader {
        let! txHash = Hash.read
        let! deltas = List.read SpendStatus.read
        let! blockNumber = Option.read readNumber4
        return { txHash = txHash; deltas = deltas; blockNumber = blockNumber }
    }

module Wallet =
    let private write ops wallet =
        Seq.write ops TxDelta.write wallet.deltas
        >> Map.write ops (fun ops (outpoint, outputStatus)-> 
            Outpoint.write ops outpoint
            >> OutputStatus.write ops outputStatus) wallet.outputs
        >> Seq.write ops (fun ops (hash, tx) ->
            ops.writeHash hash
            >> Transaction.write WithoutWitness ops tx) wallet.mempool
        >> ops.writeHash wallet.tip
        >> ops.writeNumber4 wallet.blockNumber
        >> PublicKey.write ops wallet.publicKey
        
    let private read = reader {
        let! deltas = List.read TxDelta.read
        let! outputs = Map.read <| reader {
            let! key = Outpoint.read
            let! value = OutputStatus.read
            return key, value
        }
        let! mempool = List.read <| reader {
            let! hash = Hash.read
            let! tx = Transaction.read WithoutWitness
            return hash, tx
        }
        let! tip = Hash.read
        let! blockNumber = readNumber4
        let! publicKey = PublicKey.read
        return { 
            deltas = deltas
            outputs = outputs
            mempool = mempool
            tip = tip
            blockNumber = blockNumber
            publicKey = publicKey
        }
    }

    let serialize wallet =
        write counters wallet 0ul
        |> int32
        |> create
        |> write serializers wallet
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run read
        |> function
        | Some value -> value
        | None -> failwith "could not deserialize wallet data"
        
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
        
module Secured =
    let private write ops = Seq.write ops (fun ops -> ops.writeByte)
    let private read = Array.read Byte.read
    
    let serialize secured =
        write counters secured 0ul
        |> int32
        |> create
        |> write serializers secured
        |> getBuffer
    let deserialize bytes =
        Stream (bytes, 0)
        |> run read
        |> function
        | Some value -> value
        | None -> failwith "could not deserialize secured data"
