module Consensus.BlockHeader
open Serialization
open Types

let toHex = Header.serialize >> FsBech32.Base16.encode

let fromHex hex =
    FsBech32.Base16.decode hex
    |> Option.bind Header.deserialize
