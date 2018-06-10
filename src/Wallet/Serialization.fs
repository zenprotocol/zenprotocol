module Wallet.Serialization

open FsNetMQ
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

open Consensus.Serialization
open Consensus.Serialization.Serialization

open Consensus.Hash
open Wallet.Types

module ExtendedKey = Wallet.ExtendedKey

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

module ConfirmationStatus =
    let write ops status =
        match status with
        | Unconfirmed -> ops.writeNumber1 0uy
        | Confirmed (blockNumber,blockHash,blockIndex) ->
            ops.writeNumber1 1uy
            >> ops.writeNumber4 blockNumber
            >> Hash.write ops blockHash
            >> VarInt.write ops (uint32 blockIndex)

    let read = reader {
        let! status = readNumber1

        if status = 0uy then
            return Unconfirmed
        else
            let! blockNumber = readNumber4
            let! blockHash = Hash.read
            let! blockIndex = VarInt.read

            return Confirmed (blockNumber,blockHash,int blockIndex)
    }

module Status =
    let write ops status =
        match status with
        | Unspent -> ops.writeNumber1 0uy
        | Spent (txHash,confirmationStatus) ->
            ops.writeNumber1 1uy
            >> Hash.write ops txHash
            >> ConfirmationStatus.write ops confirmationStatus

    let read = reader {
        let! status = readNumber1

        if status = 0uy then
            return Unspent
        else
            let! txHash = Hash.read
            let! confirmationStatus = ConfirmationStatus.read

            return Spent (txHash,confirmationStatus)
    }

module Output =
    let write ops (output:Wallet.Types.Output) =
        Hash.write ops output.pkHash
        >> Spend.write ops output.spend
        >> Lock.write ops output.lock
        >> Outpoint.write ops output.outpoint
        >> Status.write ops output.status
        >> ConfirmationStatus.write ops output.confirmationStatus

    let read = reader {
        let! pkHash = Hash.read
        let! spend = Spend.read
        let! lock = Lock.read
        let! outpoint = Outpoint.read
        let! status = Status.read
        let! confirmationStatus = ConfirmationStatus.read

        return {
            pkHash=pkHash
            spend=spend
            lock=lock
            outpoint=outpoint
            status=status
            confirmationStatus=confirmationStatus
        }
    }

    let serialize output =
        write counters output 0ul
        |> int32
        |> create
        |> write serializers output
        |> getBuffer

    let deserialize bytes =
        Stream (bytes, 0)
        |> run read

module AddressType =
    open DataAccess

    let write ops addressType =
        match addressType with
        | External index -> ops.writeNumber1 0uy >> VarInt.write ops (uint32 index)
        | Change index -> ops.writeNumber1 1uy >> VarInt.write ops (uint32 index)
        | Payment index -> ops.writeNumber1 2uy >> VarInt.write ops (uint32 index)
        | WatchOnly -> ops.writeNumber1 3uy

    let read = reader {
        let! addressType = readNumber1

        if addressType = 3uy then
            return WatchOnly
        else
            let! index = VarInt.read
            let index = int32 index

            match addressType with
            | 0uy -> return External index
            | 1uy -> return Change index
            | 2uy -> return Payment index
            | _ -> return! fail
    }

module Address =
    let write ops (address:Address) =
         Hash.write ops address.pkHash
         >> AddressType.write ops address.addressType

    let read = reader {
        let! pkHash = Hash.read
        let! addressType = AddressType.read

        return {
            pkHash=pkHash
            addressType=addressType
        }
    }

    let serialize output =
        write counters output 0ul
        |> int32
        |> create
        |> write serializers output
        |> getBuffer

    let deserialize bytes =
        Stream (bytes, 0)
        |> run read

module ExtendedPublicKey =
    open NBitcoin

    let write ops (publicExtendedKey:ExtendedKey.T) =
        match publicExtendedKey with
        | ExtendedKey.ExtendedPublicKey key ->
            Bytes.write ops (key.ToBytes())
        | _ -> failwith "expeded extended public key"

    let read = reader {
        let! bytes = Bytes.read

        return
            (new ExtPubKey(bytes)
            |> ExtendedKey.ExtendedPublicKey)
    }

module Account =
    let write ops account =
        Hash.write ops account.blockHash
        >> ops.writeNumber4 account.blockNumber
        >> VarInt.write ops (uint32 account.counter)
        >> ExtendedPublicKey.write ops account.publicKey
        >> Bytes.write ops account.secureMnemonicPhrase
        >> Hash.write ops account.externalPKHash
        >> Hash.write ops account.changePKHash

    let read = reader {
        let! blockHash = Hash.read
        let! blockNumber = readNumber4
        let! counter = VarInt.read
        let counter = int counter
        let! publicKey = ExtendedPublicKey.read
        let! secureMnemonicPhrase = Bytes.read
        let! externalPKHash = Hash.read
        let! changePKHash = Hash.read

        return {
            blockHash = blockHash
            blockNumber = blockNumber
            counter = counter
            publicKey = publicKey
            secureMnemonicPhrase = secureMnemonicPhrase
            externalPKHash = externalPKHash
            changePKHash = changePKHash
        }
    }

    let serialize account =
        write counters account 0ul
        |> int32
        |> create
        |> write serializers account
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