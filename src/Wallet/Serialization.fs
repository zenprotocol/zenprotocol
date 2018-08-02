module Wallet.Serialization

open FsNetMQ
open FsNetMQ.Stream

open Consensus.Serialization
open Consensus.Serialization.Serialization

open Consensus.Hash
open Wallet.Types

module ExtendedKey = Wallet.ExtendedKey

module Outpoint =
    let serialize = serialize Outpoint.size Outpoint.write

    let deserialize = deserialize Outpoint.read

module ConfirmationStatus =
    let size status =
        match status with
        | Unconfirmed -> 1
        | Confirmed (_,_,blockIndex) ->
            1 + 4 + Hash.size + VarInt.size (uint32 blockIndex)

    let write (stream:Stream) status =
        match status with
        | Unconfirmed -> stream.writeNumber1 0uy
        | Confirmed (blockNumber,blockHash,blockIndex) ->
            stream.writeNumber1 1uy
            stream.writeNumber4 blockNumber
            Hash.write stream blockHash
            VarInt.write stream (uint32 blockIndex)

    let read (stream:Stream) =
        let status = stream.readNumber1 ()

        if status = 0uy then
            Unconfirmed
        else
            let blockNumber = stream.readNumber4 ()
            let blockHash = Hash.read stream
            let blockIndex = VarInt.read stream

            Confirmed (blockNumber,blockHash,int blockIndex)

module Status =
    let size status =
        match status with
        | Unspent -> 1
        | Spent (_,confirmationStatus) -> 1 + Hash.size + ConfirmationStatus.size confirmationStatus

    let write (stream:Stream) status =
        match status with
        | Unspent -> stream.writeNumber1 0uy
        | Spent (txHash,confirmationStatus) ->
            stream.writeNumber1 1uy
            Hash.write stream txHash
            ConfirmationStatus.write stream confirmationStatus

    let read (stream:Stream) =
        let status = stream.readNumber1 ()

        if status = 0uy then
            Unspent
        else
            let txHash = Hash.read stream
            let confirmationStatus = ConfirmationStatus.read stream

            Spent (txHash,confirmationStatus)

module Output =
    let size (output:Wallet.Types.Output) =
        Hash.size +
            Spend.size output.spend +
            Lock.size output.lock +
            Outpoint.size output.outpoint +
            Status.size output.status +
            ConfirmationStatus.size output.confirmationStatus

    let write stream (output:Wallet.Types.Output) =
        Hash.write stream output.pkHash
        Spend.write stream output.spend
        Lock.write stream output.lock
        Outpoint.write stream output.outpoint
        Status.write stream output.status
        ConfirmationStatus.write stream output.confirmationStatus

    let read stream =
        let pkHash = Hash.read stream
        let spend = Spend.read stream
        let lock = Lock.read stream
        let outpoint = Outpoint.read stream
        let status = Status.read stream
        let confirmationStatus = ConfirmationStatus.read stream

        {
            pkHash=pkHash
            spend=spend
            lock=lock
            outpoint=outpoint
            status=status
            confirmationStatus=confirmationStatus
        }

    let serialize = serialize size write

    let deserialize = deserialize read

module AddressType =
    open DataAccess

    let size addressType =
        match addressType with
        | External index -> 1 + VarInt.size (uint32 index)
        | Change index -> 1 + VarInt.size (uint32 index)
        | Payment index -> 1 + VarInt.size (uint32 index)
        | WatchOnly -> 1

    let write (stream:Stream) addressType =
        match addressType with
        | External index ->
            stream.writeNumber1 0uy
            VarInt.write stream (uint32 index)
        | Change index ->
            stream.writeNumber1 1uy
            VarInt.write stream (uint32 index)
        | Payment index ->
            stream.writeNumber1 2uy
            VarInt.write stream (uint32 index)
        | WatchOnly -> stream.writeNumber1 3uy

    let read (stream:Stream) =
        let addressType = stream.readNumber1 ()

        if addressType = 3uy then
            WatchOnly
        else
            let index = VarInt.read stream |> int32

            match addressType with
            | 0uy -> External index
            | 1uy -> Change index
            | 2uy -> Payment index
            | _ -> raise SerializationException

module Address =
    let size (address:Address) = Hash.size + AddressType.size address.addressType

    let write stream (address:Address) =
         Hash.write stream address.pkHash
         AddressType.write stream address.addressType

    let read stream =
        let pkHash = Hash.read stream
        let addressType = AddressType.read stream

        {
            pkHash=pkHash
            addressType=addressType
        }

    let serialize = serialize size write

    let deserialize = deserialize read

module ExtendedPublicKey =
    open NBitcoin

    let size (publicExtendedKey:ExtendedKey.T) =
        match publicExtendedKey with
        | ExtendedKey.ExtendedPublicKey key -> Bytes.size (key.ToBytes())
        | _ -> failwith "expected extended public key"

    let write stream (publicExtendedKey:ExtendedKey.T) =
        match publicExtendedKey with
        | ExtendedKey.ExtendedPublicKey key ->
            Bytes.write stream (key.ToBytes())
        | _ -> failwith "expected extended public key"

    let read stream =
        let bytes = Bytes.read stream

        new ExtPubKey(bytes)
        |> ExtendedKey.ExtendedPublicKey

module Account =
    let size account =
        Hash.size +
            4 +
            VarInt.size (uint32 account.counter) +
            ExtendedPublicKey.size account.publicKey +
            Bytes.size account.secureMnemonicPhrase +
            Hash.size +
            Hash.size

    let write stream account =
        Hash.write stream account.blockHash
        stream.writeNumber4 account.blockNumber
        VarInt.write stream (uint32 account.counter)
        ExtendedPublicKey.write stream account.publicKey
        Bytes.write stream account.secureMnemonicPhrase
        Hash.write stream account.externalPKHash
        Hash.write stream account.changePKHash

    let read stream =
        let blockHash = Hash.read stream
        let blockNumber = stream.readNumber4 ()
        let counter = VarInt.read stream |> int
        let publicKey = ExtendedPublicKey.read stream
        let secureMnemonicPhrase = Bytes.read stream
        let externalPKHash = Hash.read stream
        let changePKHash = Hash.read stream

        {
            blockHash = blockHash
            blockNumber = blockNumber
            counter = counter
            publicKey = publicKey
            secureMnemonicPhrase = secureMnemonicPhrase
            externalPKHash = externalPKHash
            changePKHash = changePKHash
        }

    let serialize = serialize size write
    let deserialize = deserialize read

module Version =
    let private size = 4
    let private write (stream:Stream) version = stream.writeNumber4 (uint32 version)
    let private read (stream:Stream) = stream.readNumber4() |> int32

    let serialize = serialize (fun _ -> size) write
    let deserialize = deserialize read
