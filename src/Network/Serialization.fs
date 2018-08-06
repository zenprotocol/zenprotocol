module Network.Serialization

open Network.Types
open Consensus.Serialization
open Consensus.Serialization.Serialization
open Infrastructure.Timestamp

module Address =
    let size ((address:string),(timestamp:Timestamp)) =
        4 + System.Text.Encoding.ASCII.GetByteCount address + 8

    let write (stream:Stream) ((address:string),(timestamp:Timestamp)) =
        let bytes = System.Text.Encoding.ASCII.GetBytes address

        stream.writeNumber4 (bytes.Length |> uint32)
        stream.writeBytes bytes bytes.Length
        stream.writeNumber8 (timestamp |> uint64)

    let read (stream:Stream) =
        let length = stream.readNumber4 () |>int
        let address = stream.readBytes length
        let timestamp = stream.readNumber8 ()

        let address = System.Text.Encoding.ASCII.GetString(address)
        let timestamp = timestamp
        address,timestamp

    let serialize = serialize size write

module Addresses =
    let serialize : (string*Timestamp) seq -> byte array =
        serialize (Seq.sizeBody Address.size) (Seq.writeBody Address.write)

    let deserialize count = deserialize (List.readBody count Address.read)

