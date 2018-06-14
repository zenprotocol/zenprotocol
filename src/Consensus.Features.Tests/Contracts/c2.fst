open Zen.Types
open Zen.Base
open Zen.Cost
open Zen.Data

module D = Zen.Dictionary
module RT = Zen.ResultT
module Tx = Zen.TxSkeleton
module C = Zen.Cost

let main txSkeleton _ contractId command sender messageBody wallet state =
    if command = "mint" then
        match sender with //TODO: fix sender!
        | Contract contract' ->
            RT.autoFailw "unexpected sender: Contract"
        | PK pk ->
            RT.autoFailw "unexpected sender: PK"
        | Anonymous ->
            begin
                let! returnAddress = messageBody >!= tryDict
                                                 >?= D.tryFind "returnAddress"
                                                 >?= tryLock in

                let! amount = messageBody >!= tryDict
                                          >?= D.tryFind "amount"
                                          >?= tryU64 in

                match returnAddress, amount with
                | Some returnAddress, Some amount ->
                    let! asset = Zen.Asset.getDefault contractId in
                    let spend = { asset=asset; amount=amount } in
                    let pInput = Mint spend in

                    let! txSkeleton =
                        Tx.addInput pInput txSkeleton
                        >>= Tx.lockToAddress spend.asset spend.amount returnAddress in
                        RT.ok @ {
                          tx = txSkeleton;
                          message = None;
                          state = NoChange;
                        }

                | _ -> RT.autoFailw "both returnAddress and amount are required"
            end
    else
        RT.autoFailw "unsupported command"

let cf _ _ _ _ _ _ _ = 
    4 + 64 + 2 + (4 + 64 + 2 + (64 + (64 + 64 + 0))) + 57
    |> cast nat
    |> C.ret
