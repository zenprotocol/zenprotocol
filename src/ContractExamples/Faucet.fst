open Zen.Types
open Zen.Base
open Zen.Cost
open Zen.Asset

module E = Zen.Error
module ET = Zen.ErrorT
module Tx = Zen.TxSkeleton

val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 13
let cf _ _ _ _ #l _ =
    ret (64 + (l * 128 + 192) + 15 + 9 + 11)

val withNoContract: result txSkeleton -> cost (result (txSkeleton ** option message)) 9
let withNoContract result =
    match result with
    | OK txSkeleton -> ET.ret (txSkeleton,None)
    | EX e -> ET.autoFail e
    | ERR msg -> ET.autoFailw msg

val get: #l:nat -> txSkeleton -> contractHash -> option lock -> wallet l -> cost (result txSkeleton) (64 + (l * 128 + 192) + 15)
let get #l txSkeleton contractHash returnAddress (wallet:wallet l) =
  match returnAddress with
  | Some returnAddress ->
      let txSkeleton =
        Tx.lockToAddress zenAsset 10000UL returnAddress txSkeleton
        >>= Tx.fromWallet zenAsset 10000UL contractHash wallet in

      ET.of_optionT "contract doesn't have enough zens to pay you" txSkeleton
  | None ->
      ET.autoFailw "returnAddress is required"

val init: txSkeleton -> contractHash -> cost (result txSkeleton) (64 + 64 + 8)
let init txSkeleton contractHash  =
  let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in

  let txSkeleton = Tx.lockToContract zenAsset tokens contractHash txSkeleton in
  ET.retT txSkeleton

val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message))  (64 + (l * 128 + 192) + 15 + 9 + 11)
let main txSkeleton contractHash command data returnAddress #l wallet =
  if command <> "init" then
    get txSkeleton contractHash returnAddress wallet
    >>= withNoContract

  else
    autoInc (init txSkeleton contractHash >>= withNoContract)


