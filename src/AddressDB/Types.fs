module AddressDB.Types

open Consensus
open Types
open Crypto
open Types
open Hash
open Wallet.Types
open AddressDB
open Wallet.Address

type DBOutput =
    {
        address: Address
        spend: Spend
        lock: Lock
        outpoint: Outpoint
        status: Status
        confirmationStatus: ConfirmationStatus
    }

type Tip =
    {
        blockHash: Hash
        blockNumber: uint32
    }