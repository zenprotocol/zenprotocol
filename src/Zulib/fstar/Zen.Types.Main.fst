module Zen.Types.Main

open Zen.Types.TxSkeleton
open Zen.Types.Extracted
open Zen.Cost

val maxCost: nat
let maxCost = 200

noeq type costFunction =
    | CostFunc:
        #n:nat{n<=maxCost}
        -> f:(txSkeleton
              -> command:string
              -> nat `cost` n)
        -> costFunction

noeq type mainFunction =
    | MainFunc:
        cf:costFunction
        -> mf:( txSkel:txSkeleton
                -> contractHash
                -> command:string
                -> result txSkeleton `cost` force ((CostFunc?.f cf) txSkel command)
              )
        -> mainFunction
