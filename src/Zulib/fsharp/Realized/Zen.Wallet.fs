module Zen.Wallet
open FStar.Pervasives
open FStar.Pervasives.Native
open Zen.Types.Extracted
open Zen.Types.Realized
open FSharp.Core.Operators.Checked

module Cost = Zen.Cost.Realized
module U64 = FStar.UInt64

type t = Zen.Types.Realized.wallet

let size : t ->  Prims.nat = Prims.length

let rec collect (asset:asset) (amount:U64.t) (wallet:wallet) collected (collectedAmount:U64.t) =
    match wallet with
    | Prims.Nil ->
        if collectedAmount >= amount then
            (Zen.List.rev collected |> Cost.__force), collectedAmount
        else
            Prims.Nil, 0UL
    | Prims.Cons (head,tail) ->
        let input, output = head

        if amount > collectedAmount && asset = output.spend.asset then
            let collected = Prims.Cons (head,collected)
            let collectedAmount = collectedAmount + output.spend.amount

            collect asset amount tail collected collectedAmount
        else
            collect asset amount tail collected collectedAmount

//let getInputs (n:Prims.nat) (asset:asset) (amount:U64.t) (wallet:wallet<Prims.unit>) (txSkeleton:txSkeleton) =
//    lazy (
//        let n,inputs,collectedAmount = collect asset amount wallet 0I V.VNil 0UL
//
//        match inputs with
//        | V.VNil -> None
//        | _ ->
//            addInputs n inputs txSkeleton
//            |> Cost.__force
//            |> Some
//    ) |> Cost.C
