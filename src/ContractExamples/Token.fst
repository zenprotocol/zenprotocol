open Zen.Types
open Zen.Base
open Zen.Cost
open Zen.Asset
open Zen.Data

module D = Zen.Dictionary
module W = Zen.Wallet
module RT = Zen.ResultT
module Tx = Zen.TxSkeleton
module C = Zen.Cost
module CR = Zen.ContractResult

let buy txSkeleton contractId returnAddress =
  let! contractToken = Zen.Asset.getDefault contractId in
  let! amount = Tx.getAvailableTokens zenAsset txSkeleton in

  let! txSkeleton =
    Tx.lockToContract zenAsset amount contractId txSkeleton
    >>= Tx.mint amount contractToken
    >>= Tx.lockToAddress contractToken amount returnAddress in

  CR.ofTxSkel txSkeleton

let redeem txSkeleton contractId returnAddress wallet =
  let! contractToken = Zen.Asset.getDefault contractId in
  let! amount = Tx.getAvailableTokens contractToken txSkeleton in

  let! txSkeleton =
    Tx.destroy amount contractToken txSkeleton
    >>= Tx.lockToAddress zenAsset amount returnAddress
    >>= Tx.fromWallet zenAsset amount contractId wallet in

  CR.ofOptionTxSkel "This contract doesn't have enough ZP in order to redeem the requested amount" txSkeleton

let main txSkeleton _ contractId command sender messageBody wallet state =
  let! returnAddress =
    messageBody >!= tryDict
                >?= D.tryFind "returnAddress"
                >?= tryLock
  in
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

let cf _ _ _ _ _ wallet _ =
    4 + 64 + 2 + (64 + (64 + (64 + 64 + (Zen.Wallet.size wallet * 128 + 192) + 3)) + 25) + 31
    |> C.ret #nat
