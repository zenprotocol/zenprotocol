module Blockchain.Tally.Types

open Consensus
open UtxoSet
open Crypto
open Types


type UpdateOperation =
    | Add
    | Remove

type Interval = uint32

type PKHash = Hash.Hash

type PKBalance = Map<PKHash,uint64>

type Allocation = byte
type Payout = Recipient * Spend list

type PKAllocation= Map<PublicKey,Allocation>
type PKPayout = Map<PublicKey,Payout>

type Candidates = Payout list

type NomineesBalance = Map<Payout,uint64>
