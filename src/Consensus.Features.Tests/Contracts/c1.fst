open Zen.Types
open Zen.Base
open Zen.Cost

module RT = Zen.ResultT
module Tx = Zen.TxSkeleton

let main txSkeleton _ contractHash command sender data wallet =
  let! asset = Zen.Asset.getDefault contractHash in
  let spend = { asset=asset; amount=1000UL } in
  let lock = ContractLock contractHash in

  let output = { lock=lock; spend=spend } in
  let pInput = Mint spend in

  let! txSkeleton =
    Tx.addInput pInput txSkeleton
    >>= Tx.lockToContract spend.asset spend.amount contractHash in

  RT.ok (txSkeleton, None)

val cf: txSkeleton -> context -> string -> sender -> option data -> wallet -> cost nat 9
let cf _ _ _ _ _ _ = ret (64 + (64 + 64 + 0) + 23)
