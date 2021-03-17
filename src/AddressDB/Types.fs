module AddressDB.Types

open Consensus
open Types
open Hash
open Wallet.Address

type ConfirmationStatus =
    | Confirmed of uint64 * uint32 * Hash * int // Block timestamp, blockNumber,blockHash,blockIndex (tx index within block)
    | Unconfirmed
    
type Status =
    | Spent of Hash * ConfirmationStatus
    | Unspent
    
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
    
type WitnessPoint = Outpoint // pointing to a witness within a transacion using its index