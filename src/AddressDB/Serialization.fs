module AddressDB.Serialization

open Consensus
open Serialization
open Serialization
open Wallet.Serialization

open AddressDB.Types
open Wallet.Address

module Address =
    [<Literal>]
    let private SerializedPK = 1uy
    [<Literal>]
    let private SerializedContract = 2uy

    let size = function
        | PK _ ->
            Byte.size + Hash.size
        |  Contract contractID ->
           Byte.size + ContractId.size contractID

    let write stream = function
        | PK pkHash ->
            Byte.write stream SerializedPK
            Hash.write stream pkHash
        | Contract contractID ->
            Byte.write stream SerializedContract
            ContractId.write stream contractID
    let read reader =
        let discriminator = Byte.read reader
        match discriminator with
        | SerializedPK ->
            let pkHash = Hash.read reader
            PK pkHash
        | SerializedContract ->
            let contractID = ContractId.read reader
            Contract contractID
        | _ ->
            raise SerializationException

    let serialize = serialize size write
    let deserialize = deserialize read

module Asset =
        let serialize = serialize Asset.size Asset.write
        let deserialize = deserialize Asset.read

module Output =
    let size output =
        Address.size output.address
        + Spend.size output.spend
        + Lock.size output.lock
        + Outpoint.size output.outpoint
        + Status.size output.status
        + ConfirmationStatus.size output.confirmationStatus

    let write stream output =
        Address.write stream output.address
        Spend.write stream output.spend
        Lock.write stream output.lock
        Outpoint.write stream output.outpoint
        Status.write stream output.status
        ConfirmationStatus.write stream output.confirmationStatus

    let read stream =
        let address = Address.read stream
        let spend = Spend.read stream
        let lock = Lock.read stream
        let outpoint = Outpoint.read stream
        let status = Status.read stream
        let confirmationStatus = ConfirmationStatus.read stream

        {
            address = address
            spend = spend
            lock = lock
            outpoint = outpoint
            status = status
            confirmationStatus = confirmationStatus
        }

    let serialize = serialize size write
    let deserialize = deserialize read

module Tip =
    let size _ =
        Hash.size + 4

    let write stream tip =
        Hash.write stream tip.blockHash
        stream.writeNumber4 tip.blockNumber

    let read stream =
        let blockHash = Hash.read stream
        let blockNumber = stream.readNumber4 ()

        {
            blockHash = blockHash
            blockNumber = blockNumber
        }

    let serialize = serialize size write
    let deserialize = deserialize read

module ContractData =
    let size (command, messageBody) =
        String.size command + 
        Option.size Data.size messageBody

    let write stream (command, messageBody) =
        String.write stream command
        Option.write stream Data.write messageBody

    let read reader =
        let command = String.read reader
        let messageBody = Option.read Data.read reader
        command, messageBody

    let serialize = serialize size write
    let deserialize = deserialize read
    