module Wallet.Types

open Consensus.Types
open Consensus.Crypto
open Consensus.Types
open Consensus.Hash
open Wallet

type AddressType =
    | External of index:int
    | Change of index:int
    | Payment of index:int
    | WatchOnly

type ConfirmationStatus =
    | Confirmed of uint32 * Hash * int
    | Unconfirmed

type Status =
    | Spent of Hash * ConfirmationStatus
    | Unspent

type Address =
    {
        pkHash:Hash
        addressType:AddressType
    }

// TODO:Saving the index of the tx inside block
type Output =
    {
        pkHash:Hash
        spend:Spend
        lock:Lock
        outpoint:Outpoint
        status:Status
        confirmationStatus:ConfirmationStatus
    }

type Account =
    {
        blockHash: Hash
        blockNumber: uint32
        counter: int32
        publicKey: ExtendedKey.T
        secureMnemonicPhrase: byte array
        externalPKHash: Hash
        changePKHash: Hash
    }