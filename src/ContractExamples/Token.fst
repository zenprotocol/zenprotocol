open Zen.Types
open Zen.Vector
open Zen.Base
open Zen.Cost
open Zen.Asset
open Zen.Data

module ET = Zen.ErrorT
module OT = Zen.OptionT
module Tx = Zen.TxSkeleton

let buy txSkeleton contractHash returnAddress =
  let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in

  let! contractAsset = getDefault contractHash in

  let! txSkeleton =
    Tx.lockToContract zenAsset tokens contractHash txSkeleton
    >>= Tx.mint tokens contractAsset
    >>= Tx.lockToAddress contractAsset tokens returnAddress in

  ret <| OK (txSkeleton, None)

let redeem txSkeleton contractHash returnAddress wallet =
  let! contractAsset = getDefault contractHash in
  let! tokens = Tx.getAvailableTokens contractAsset txSkeleton in

  let! txSkeleton =
    Tx.destroy tokens contractAsset txSkeleton
    >>= Tx.lockToAddress zenAsset tokens returnAddress
    >>= Tx.fromWallet zenAsset tokens contractHash wallet in

  match txSkeleton with
  | Some txSkeleton -> ret <| OK (txSkeleton, None)
  | None -> ret <| ERR "contract doesn't have enough zens to pay you"

val main: txSkeleton -> hash -> string -> option data -> wallet:wallet -> cost (result (txSkeleton ** option message)) (3 + 66 + (64 + (64 + (64 + 64 + (Zen.Wallet.size wallet * 128 + 192) + 0)) + 31) + 28)
let main txSkeleton contractHash command data wallet =
  let! returnAddress = data >!> tryDict >?> tryFindLock "returnAddress" in

  match returnAddress with
  | Some returnAddress ->
      if command = "redeem" then
        redeem txSkeleton contractHash returnAddress wallet
      else if command = "" || command = "buy" then
        buy txSkeleton contractHash returnAddress
        |> autoInc
      else
        ET.autoFailw "unsupported command"
  | None ->
      ET.autoFailw "returnAddress is required"

val cf: txSkeleton -> string -> option data -> wallet -> cost nat 24
let cf _ _ _ wallet = ret (3 + 66 + (64 + (64 + (64 + 64 + (Zen.Wallet.size wallet * 128 + 192) + 0)) + 31) + 28)
