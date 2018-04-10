module Wallet.Secured

open Consensus.Crypto
open Infrastructure.Result
open Infrastructure.Security

type T = byte[]

let create password secret = AuthenticatedEncryption.encrypt password secret

let decrypt password secured = AuthenticatedEncryption.decrypt password secured >>= ExtendedKey.create