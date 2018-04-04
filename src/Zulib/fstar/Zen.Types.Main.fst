module Zen.Types.Main

open Zen.Types.Extracted
open Zen.Types.Realized
open Zen.Cost
open Zen.Types.Data


val maxCost: nat
let maxCost = 200

type message =
    { cHash: hash;
      command: string;
      data: option data }

(*
type contractArgs = {
    cHash: hash;
    command: string;
    data: data;
}
*)

type contractResult = result (txSkeleton ** option message)

noeq type costFunction =
    | CostFunc:
        #n:nat{n<=maxCost}
        -> f:(txSkeleton
              -> command:string
              -> data:option data
              -> wallet
              -> nat `cost` n)
        -> costFunction

noeq type mainFunction =
    | MainFunc:
        cf:costFunction
        -> mf:( txSkel:txSkeleton
                -> contractHash
                -> command:string
                -> data:option data
                -> wallet:wallet
                -> contractResult `cost` force ((CostFunc?.f cf) txSkel command data wallet)
              )
        -> mainFunction
