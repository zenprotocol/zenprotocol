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

let main txSkeleton context contractHash command sender data wallet =
  let! returnAddress = data >!= tryCollection >?= tryDict >?= tryFind "returnAddress" >?= tryLock in

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

let cf _ _ _ _ _ wallet = ret  ((2 + 2 + 64 + 2 + (64 + (64 + (64 + 64 + (Zen.Wallet.size wallet * 128 + 192) + 0)) + 25) + 33) <: nat)
