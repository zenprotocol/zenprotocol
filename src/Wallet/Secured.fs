module Wallet.Secured

open Consensus.Crypto
open Infrastructure.Result
open Infrastructure.Security

type T = byte[]

let create password (secret:string) = AuthenticatedEncryption.encrypt password (System.Text.Encoding.ASCII.GetBytes secret)

let decrypt password secured = AuthenticatedEncryption.decrypt password secured <@> System.Text.Encoding.ASCII.GetString >>= ExtendedKey.fromMnemonicPhrase