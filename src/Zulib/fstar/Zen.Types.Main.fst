module Zen.Types.Main

open Zen.Types.Extracted
open Zen.Types.Realized
open Zen.Cost


val maxCost: nat
let maxCost = 200

type message =
    { cHash: hash;
      command: string } //TODO: data

noeq type costFunction =
    | CostFunc:
        #n:nat{n<=maxCost}
        -> f:(txSkeleton
              -> command:string
              -> returnAddress:option lock
              -> #l:nat
              -> wallet l
              -> nat `cost` n)
        -> costFunction

noeq type mainFunction =
    | MainFunc:
        cf:costFunction
        -> mf:( txSkel:txSkeleton
                -> contractHash
                -> command:string
                -> returnAddress:option lock
                -> #l:nat
                -> wallet:wallet l
                -> result (txSkeleton ** option message) `cost` force ((CostFunc?.f cf) txSkel command returnAddress wallet)
              )
        -> mainFunction
