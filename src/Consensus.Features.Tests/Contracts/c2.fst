open Zen.Types
open Zen.Base
open Zen.Cost
open Zen.Data

module D = Zen.Dictionary
module RT = Zen.ResultT
module Tx = Zen.TxSkeleton

let main txSkeleton _ contractHash command sender data wallet =
    if command = "mint" then
        match sender with //TODO: fix sender!
        | Contract contract' ->
            RT.autoFailw "unexpected sender: Contract"
        | PK pk ->
            RT.autoFailw "unexpected sender: PK"
        | Anonymous ->
            begin
                let! returnAddress = data >!= tryCollection
                                          >?= tryDict
                                          >?= D.tryFind "returnAddress"
                                          >?= tryLock in

                let! amount = data >!= tryCollection
                                   >?= tryDict
                                   >?= D.tryFind "amount"
                                   >?= tryU64 in

                match returnAddress, amount with
                | Some returnAddress, Some amount ->
                    let! asset = Zen.Asset.getDefault contractHash in
                    let spend = { asset=asset; amount=amount } in
                    let pInput = Mint spend in
                    
                    let! txSkeleton =
                        Tx.addInput pInput txSkeleton
                        >>= Tx.lockToAddress spend.asset spend.amount returnAddress in
                        RT.ok (txSkeleton, None)
                | _ -> RT.autoFailw "both returnAddress and amount are required"
            end
    else
        RT.autoFailw "unsupported command"

val cf: txSkeleton -> context -> string -> sender -> option data -> wallet -> cost nat 25
let cf _ _ _ _ _ _ = ret (2 + 2 + 64 + 2 + (2 + 2 + 64 + 2 + (64 + (64 + 64 + 0))) + 59)