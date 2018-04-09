open Zen.Types
open Zen.Base
open Zen.Cost
open Zen.Asset
open Zen.Data

module RT = Zen.ResultT
module OT = Zen.OptionT
module Tx = Zen.TxSkeleton
module CR = Zen.ContractResult.NoMessage

let buy txSkeleton contractHash returnAddress =
  let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in

  let! contractAsset = getDefault contractHash in

  let! txSkeleton =
    Tx.lockToContract zenAsset tokens contractHash txSkeleton
    >>= Tx.mint tokens contractAsset
    >>= Tx.lockToAddress contractAsset tokens returnAddress in

  CR.ret txSkeleton

let redeem txSkeleton contractHash returnAddress wallet =
  let! contractAsset = getDefault contractHash in
  let! tokens = Tx.getAvailableTokens contractAsset txSkeleton in

  let! txSkeleton =
    Tx.destroy tokens contractAsset txSkeleton
    >>= Tx.lockToAddress zenAsset tokens returnAddress
    >>= Tx.fromWallet zenAsset tokens contractHash wallet in

  CR.ofOption "contract doesn't have enough zens to pay you" txSkeleton

val main: txSkeleton -> hash -> string -> sender -> option data -> wallet:wallet -> cost (result (txSkeleton ** option message))  (2 + 66 + (64 + (64 + (64 + 64 + (Zen.Wallet.size wallet * 128 + 192) + 0)) + 25) + 29)
let main txSkeleton contractHash command sender data wallet =
  let! returnAddress = data >!> tryDict >?> tryFindLock "returnAddress" in

  match returnAddress with
  | Some returnAddress ->
      if command = "redeem" then
        redeem txSkeleton contractHash returnAddress wallet
      else if command = "" || command = "buy" then
        buy txSkeleton contractHash returnAddress
        |> autoInc
      else
        RT.autoFailw "unsupported command"
  | None ->
      RT.autoFailw "returnAddress is required"

val cf: txSkeleton -> string -> sender -> option data -> wallet -> cost nat 24
let cf _ _ _ _ wallet = ret  (2 + 66 + (64 + (64 + (64 + 64 + (Zen.Wallet.size wallet * 128 + 192) + 0)) + 25) + 29)
