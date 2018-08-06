module AddressDB.Serialization

open FsNetMQ.Stream

open Consensus.Serialization
open Consensus.Serialization.Serialization
open Wallet.Serialization

open AddressDB.Types
open Wallet.Address

module Address =
    [<Literal>]
    let private SerializedPK = 1uy
    [<Literal>]
    let private SerializedContract = 2uy

    let write ops = function
        | PK pkHash ->
            Byte.write ops SerializedPK
            >> Hash.write ops pkHash
        | Contract contractID ->
            Byte.write ops SerializedContract
            >> ContractId.write ops contractID
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
    
    let serialize address =
        write counters address 0ul
        |> int32
        |> create
        |> write serializers address
        |> getBuffer

    let deserialize bytes =
        Reader (bytes)
        |> run read

module Output =
    let write ops output =
        Address.write ops output.address
        >> Spend.write ops output.spend
        >> Lock.write ops output.lock
        >> Outpoint.write ops output.outpoint
        >> Status.write ops output.status
        >> ConfirmationStatus.write ops output.confirmationStatus

    let read reader =
        let address = Address.read reader
        let spend = Spend.read reader
        let lock = Lock.read reader
        let outpoint = Outpoint.read reader
        let status = Status.read reader
        let confirmationStatus = ConfirmationStatus.read reader

        {
            address = address
            spend = spend
            lock = lock
            outpoint = outpoint
            status = status
            confirmationStatus = confirmationStatus
        }

    let serialize output =
        write counters output 0ul
        |> int32
        |> create
        |> write serializers output
        |> getBuffer

    let deserialize bytes =
        Reader (bytes)
        |> run read

module Account =
    let write ops account =
        Hash.write ops account.blockHash
        >> ops.writeNumber4 account.blockNumber

    let read reader =
        let blockHash = Hash.read reader
        let blockNumber = reader.readNumber4 () 

        {
            blockHash = blockHash
            blockNumber = blockNumber
        }

    let serialize account =
        write counters account 0ul
        |> int32
        |> create
        |> write serializers account
        |> getBuffer

    let deserialize bytes =
        Reader (bytes)
        |> run read