module Network.TransactionPublisher

open Consensus
open Consensus.Hash
open Consensus.Types

type T = Hash list

let empty = List.empty

let add txHash (publisher:T) =
    txHash :: publisher

let tick transport (publisher:T) : T =
    let txHashes =
        List.rev publisher
        |> List.map Hash.bytes
        |> Array.concat

    Transport.Transport.publishTransactions transport txHashes

    []