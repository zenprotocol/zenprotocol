module Zen.TxSkeleton

open Zen.Base
open Zen.Cost
open Zen.Types

module W = Zen.Wallet
module U64 = FStar.UInt64

val getAvailableTokens: asset -> txSkeleton -> U64.t `cost` 64

val addInput: input -> txSkeleton -> txSkeleton `cost` 64

(*
val addInput_AvailableTokens:
    pOut:pointedOutput
    -> txSkel: txSkeleton
    -> Lemma ( let open U64 in
               let spend = (snd pOut).spend in
               let asset = spend.asset in
               let txSkel' = addInput (PointedOutput pOut) txSkel
                             |> force in
               let previouslyAvailableTokens = getAvailableTokens asset txSkel
                                               |> force in
               let availableTokens = getAvailableTokens asset txSkel'
                                     |> force in
               availableTokens = previouslyAvailableTokens +%^ spend.amount
             )
*)

val addInputs:
  inputs: list input
  -> txSkeleton
  -> txSkeleton `cost` (length inputs * 64 + 64)

(*
assume AddInputs_is_fold:
    forall (#n:nat) (inputs: input `V.t` n) (txSkel: txSkeleton).
        force (addInputs inputs txSkel)
        ==
        force (V.foldl (flip addInput) txSkel inputs)
*)

val lockToContract:
  asset
  -> U64.t
  -> contractId
  -> txSkeleton
  -> txSkeleton `cost` 64

val lockToPubKey:
  asset
  -> U64.t
  -> pkHash:hash
  -> txSkeleton
  -> txSkeleton `cost` 64

val lockToAddress:
  asset
  -> U64.t
  -> address:lock
  -> txSkeleton
  -> txSkeleton `cost` 64

val addChangeOutput:
  asset
  -> contractId
  -> txSkeleton
  -> txSkeleton `cost` 64

val mint:
  amount:U64.t
  -> asset
  -> txSkeleton
  -> txSkeleton `cost` 64

val destroy:
  amount:U64.t
  -> asset
  -> txSkeleton
  -> txSkeleton `cost` 64

val fromWallet:
  asset ->
  amount:U64.t ->
  contractId ->
  wallet: wallet ->
  txSkeleton ->
  option txSkeleton `cost` (W.size wallet * 128 + 192)

val isValid: txSkeleton -> bool `cost` 64
