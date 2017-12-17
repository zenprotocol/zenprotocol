module Zen.Wallet

open Zen.Base
open Zen.Cost
open Zen.Types
open FStar.UInt64

module V = Zen.Vector

assume new type wallet
assume Wallet_Eq: hasEq wallet

val getTransaction: wallet -> transactionSkeleton

val getFunds: wallet -> (asset:hash) -> UInt64.t

val getFundsFrom: wallet -> (pkhash:hash) -> (asset:hash) -> UInt64.t

assume GetFundsFrom_GetFunds:
  forall (w:wallet) (asset:hash) (pkhash:hash).
  getFundsFrom w pkhash asset <=^ w `getFunds` asset

val addOutput: w:wallet
  -> o: output{o.spend.amount <=^ w `getFunds` o.spend.asset}
  -> wallet

(* If a wallet can spend an output o with amount m of asset a, then spending o:
  1) Increases the length of the transactionSkeleton outputs by 1
  2) Results in the last transactionSkeleton output being equal to o *)
assume AddOutput_GetTransaction:
  forall (w:wallet) (o:output{o.spend.amount <=^ w `getFunds` o.spend.asset}).
  let num_outputs = Tx?.l2 @ getTransaction w in
  match getTransaction (addOutput w o) with
  | Tx #_ _ #l outputs _ ->
    l = num_outputs + 1 /\ force (V.nth outputs (l-1)) == o

(*
  If a wallet can spend an output o with amount m of asset a, then spending o:
  1) Reduces the amount of asset a in the wallet by m
*)
assume AddOutput_getFunds:
  forall (w:wallet) (o:output{o.spend.amount <=^ w `getFunds` o.spend.asset}).
  getFunds (w `addOutput` o) o.spend.asset = getFunds w o.spend.asset -^ o.spend.amount

val getOwner: wallet -> owner:hash

(*
  If a wallet can spend an output o with amount m of asset a, then spending o:
  1) Increases the length of the transactionSkeleton outputs by 1
  2) Results in the last transactionSkeleton output being equal to o
*)
assume AddOutput_getOwner:
  forall w (o:output{o.spend.amount <=^ w `getFunds` o.spend.asset}).
  getOwner (w `addOutput` o) = getOwner w
