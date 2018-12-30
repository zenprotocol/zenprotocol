module Consensus.InputValidation.Coinbase

open Consensus
open Consensus.ValidationError
open Consensus.Chain
open Consensus.TxSkeleton
open State

module PublicKey = Crypto.PublicKey

let validate chainParams blockNumber contractState txHash witnesses pkHash coinbaseBlockNumber inputs =
    if blockNumber - coinbaseBlockNumber < chainParams.coinbaseMaturity then
        Invalid <| General "Coinbase not mature enough"
    else
        PK.validate contractState txHash witnesses pkHash inputs