module Zen.Wallet
open FStar.Pervasives
open FStar.Pervasives.Native
open Zen.Types.Extracted
open Zen.Types.Realized

module Cost = Zen.Cost.Realized
module V = Zen.Vector
module U64 = FStar.UInt64

let rec collect (asset:asset) (amount:U64.t) (wallet:wallet<Prims.unit>) (n1:Prims.nat) collected (collectedAmount:U64.t) =
    match wallet with
    | V.VNil -> 
        if collectedAmount >= amount then 
            n1,collected,collectedAmount
        else
            0I,V.VNil,0UL
    | V.VCons (length,head,tail) ->
        let input, output = head
                 
        if amount > collectedAmount && asset = output.spend.asset then
            let collected = V.VCons (n1,head,collected)
            let collectedAmount = collectedAmount + output.spend.amount
            let n1 = n1 + 1I
            
            collect asset amount tail n1 collected collectedAmount
        else 
            collect asset amount tail n1 collected collectedAmount

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
                




