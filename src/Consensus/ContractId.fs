module Consensus.ContractId

open Consensus.Types
open Infrastructure
open FsBech32

let toBytes (ContractId (version,cHash)) =
    Array.append (BigEndianBitConverter.uint32ToBytes version) (Hash.bytes cHash)

let fromBytes bytes =
    if Array.length bytes <> 36 then
        None
    else

    let version,cHash = Array.splitAt 4 bytes

    let version = BigEndianBitConverter.toUint32 version
    let cHash = Hash.Hash cHash

    Some <| ContractId (version,cHash)

let toString (contractId:ContractId) = contractId.ToString()

let fromString b16 =
    Base16.decode b16
    |> Option.map fromBytes

let contractHash (ContractId (_,cHash)) = cHash