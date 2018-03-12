open Zen.Types
open Zen.Vector
open Zen.Base
open Zen.Cost
open Zen.Asset

module ET = Zen.ErrorT
module OT = Zen.OptionT
module Tx = Zen.TxSkeleton

val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 19
let cf _ _ _ _ #l _ = ret (64 + (64 + (64 + 64 + (l * 128 + 192) + 0)) + 28 + 22)

let buy txSkeleton contractHash returnAddress =
  let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in

  let! contractAsset = getDefault contractHash in

  let! txSkeleton =
    Tx.lockToContract zenAsset tokens contractHash txSkeleton
    >>= Tx.mint tokens contractAsset
    >>= Tx.lockToAddress contractAsset tokens returnAddress in

  ret <| OK (txSkeleton, None)

let redeem #l txSkeleton contractHash returnAddress (wallet:wallet l) =
  let! contractAsset = getDefault contractHash in
  let! tokens = Tx.getAvailableTokens contractAsset txSkeleton in

  let! txSkeleton =
    Tx.destroy tokens contractAsset txSkeleton
    >>= Tx.lockToAddress zenAsset tokens returnAddress
    >>= Tx.fromWallet zenAsset tokens contractHash wallet in

  match txSkeleton with
  | Some txSkeleton -> ret <| OK (txSkeleton, None)
  | None -> ret <| ERR "contract doesn't have enough zens to pay you"

val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message)) (64 + (64 + (64 + 64 + (l * 128 + 192) + 0)) + 28 + 22)
let main txSkeleton contractHash command data returnAddress #l wallet =
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

