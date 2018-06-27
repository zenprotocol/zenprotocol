module Consensus.InputValidation.PK

open Consensus
open ValidationError
open Types
open TxSkeleton
open State

module PublicKey = Crypto.PublicKey

let validate contractState txHash witnesses pkHash inputs =
    match witnesses with
    | PKWitness (sigHash, publicKey, signature) :: tail ->
        if PublicKey.hash publicKey = pkHash then
            let msg =
                match sigHash with
                | TxHash -> Some txHash
                | FollowingWitnesses ->
                    let witnessesHash = Serialization.Witnesses.hash tail
                    Hash.joinHashes [ txHash; witnessesHash ] |> Some
                | UnknownSigHash _ -> None

            match msg with
            | Some msg ->
                match Crypto.verify publicKey signature msg with
                | Crypto.Valid -> NextInput (tail, inputs, contractState)
                | _ -> Invalid <| General "invalid PK witness signature"
            | None ->
                Invalid <| General "Unknown sighash"
        else Invalid <| General "PK witness mismatch"
    | _ -> Invalid <| General "expecting a public key witness"
