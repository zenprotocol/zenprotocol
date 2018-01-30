module Zen.Types.Main

open Zen.Types.Extracted
open Zen.Types.Realized
open Zen.Cost


val maxCost: nat
let maxCost = 200

noeq type costFunction =
    | CostFunc:
        #n:nat{n<=maxCost}
        -> f:(txSkeleton
              -> command:string
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
                -> #l:nat
                -> wallet:wallet l
                -> result txSkeleton `cost` force ((CostFunc?.f cf) txSkel command wallet)
              )
        -> mainFunction
