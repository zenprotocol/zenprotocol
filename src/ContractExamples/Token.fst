open Zen.Types
open Zen.Base
open Zen.Cost
open Zen.Asset
open Zen.Data
open Zen.Dictionary

module RT = Zen.ResultT
module OT = Zen.OptionT
module Tx = Zen.TxSkeleton
module CR = Zen.ContractResult.NoMessage

let buy txSkeleton contractId returnAddress =
  let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in

  let! contractAsset = getDefault contractId in

  let! txSkeleton =
    Tx.lockToContract zenAsset tokens contractId txSkeleton
    >>= Tx.mint tokens contractAsset
    >>= Tx.lockToAddress contractAsset tokens returnAddress in

  CR.ret txSkeleton

let redeem txSkeleton contractId returnAddress wallet =
  let! contractAsset = getDefault contractId in
  let! tokens = Tx.getAvailableTokens contractAsset txSkeleton in

  let! txSkeleton =
    Tx.destroy tokens contractAsset txSkeleton
    >>= Tx.lockToAddress zenAsset tokens returnAddress
    >>= Tx.fromWallet zenAsset tokens contractId wallet in

  CR.ofOption "contract doesn't have enough zens to pay you" txSkeleton

let main txSkeleton context contractId command sender data wallet =
  let! returnAddress = data >!= tryDict >?= tryFind "returnAddress" >?= tryLock in

  match returnAddress with
  | Some returnAddress ->
      if command = "redeem" then
        redeem txSkeleton contractId returnAddress wallet
      else if command = "" || command = "buy" then
        buy txSkeleton contractId returnAddress
        |> autoInc
      else
        RT.autoFailw "unsupported command"
  | None ->
      RT.autoFailw "returnAddress is required"

let cf _ _ _ _ _ wallet =
    (4 + 64 + 2 + (64 + (64 + (64 + 64 + (Zen.Wallet.size wallet * 128 + 192) + 0)) + 25) + 31)
    |> cast nat
    |> ret
