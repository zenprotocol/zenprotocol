module Consensus.InputValidation.Coinbase

open Consensus
open Consensus.ValidationError
open Consensus.Types
open Consensus.TxSkeleton
open State

module PublicKey = Crypto.PublicKey

let validate blockNumber txHash witnesses pkHash coinbaseBlockNumber inputs =
    if blockNumber - coinbaseBlockNumber < CoinbaseMaturity then
        Invalid <| General "Coinbase not mature enough"
    else
        PK.validate txHash witnesses pkHash inputs