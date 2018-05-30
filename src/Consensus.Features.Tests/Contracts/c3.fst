open Zen.Types
open Zen.Base
open Zen.Cost
open Zen.Asset
open Zen.Data
open FStar.UInt32

module RT = Zen.ResultT
module Tx = Zen.TxSkeleton
module Crypto = Zen.Crypto
module C = Zen.Cost

let main txSkeleton _ contractId _ _ _ _ state =
    let! asset = Zen.Asset.getDefault contractId in
    let spend = { asset=asset; amount=1000UL } in
    let lock = ContractLock contractId in

    let output = { lock=lock; spend=spend } in
    let pInput = Mint spend in

    let! txSkeleton =
        Tx.addInput pInput txSkeleton
        >>= Tx.lockToContract spend.asset spend.amount contractId in

    let! counter = state >!= tryU32 in
    let updatedCounter =
        match counter with
        | Some counter -> counter +%^ 1ul
        | None -> 10ul in
        
    RT.ok @ { tx = txSkeleton; message = None; state = Update (U32 updatedCounter) }

let cf _ _ _ _ _ _ _ =
    64 + (64 + 64 + (2 + 0)) + 36
    |> cast nat
    |> C.ret