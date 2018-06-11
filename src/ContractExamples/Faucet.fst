open Zen.Types
open Zen.Base
open Zen.Cost
open Zen.Asset

module E = Zen.Error
module ET = Zen.ErrorT
module Tx = Zen.TxSkeleton
module C = Zen.Cost

let cf _ _ _ _ #l _ =
    64 + (l * 128 + 192) + 15 + 9 + 11
    |> cast nat
    |> C.ret

val withNoContract: result txSkeleton -> cost (result (txSkeleton ** option message)) 9
let withNoContract result =
    match result with
    | OK txSkeleton -> ET.ret (txSkeleton,None)
    | EX e -> ET.autoFail e
    | ERR msg -> ET.autoFailw msg

val get: #l:nat -> txSkeleton -> contractId -> option lock -> wallet l -> cost (result txSkeleton) (64 + (l * 128 + 192) + 15)
let get #l txSkeleton contractId returnAddress (wallet:wallet l) =
  match returnAddress with
  | Some returnAddress ->
      let txSkeleton =
        Tx.lockToAddress zenAsset 10000UL returnAddress txSkeleton
        >>= Tx.fromWallet zenAsset 10000UL contractId wallet in

      ET.of_optionT "contract doesn't have enough zens to pay you" txSkeleton
  | None ->
      ET.autoFailw "returnAddress is required"

val init: txSkeleton -> contractId -> cost (result txSkeleton) (64 + 64 + 8)
let init txSkeleton contractId  =
  let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in

  let txSkeleton = Tx.lockToContract zenAsset tokens contractId txSkeleton in
  ET.retT txSkeleton

let main txSkeleton contractId command data returnAddress #l wallet =
  if command <> "init" then
    get txSkeleton contractId returnAddress wallet
    >>= withNoContract

  else
    autoInc (init txSkeleton contractId >>= withNoContract)


