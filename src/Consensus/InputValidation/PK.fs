module Consensus.InputValidation.PK

open Consensus
open Consensus.ValidationError
open Consensus.Types
open Consensus.TxSkeleton
open State

module PublicKey = Crypto.PublicKey

let validate contractState txHash witnesses pkHash inputs =
    match witnesses with
    | PKWitness (publicKey,signature) :: tail ->
        if PublicKey.hash publicKey = pkHash then
            match Crypto.verify publicKey signature txHash with
            | Crypto.Valid -> NextInput (tail, inputs, contractState)
            | _ -> Invalid <| General "invalid PK witness signature"
        else Invalid <| General "PK witness mismatch"
    | _ -> Invalid <| General "expecting a public key witness"
