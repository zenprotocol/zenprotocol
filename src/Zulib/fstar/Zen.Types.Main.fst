module Zen.Types.Main

open Zen.Types.TxSkeleton
open Zen.Types.Extracted

val maxCost: nat
let maxCost = 200

noeq type costFunction =
    | CostFunc:
        #n:nat{n<=maxCost}
        -> f:(txSkeleton -> Zen.Cost.t nat n)
        -> costFunction

noeq type mainFunction =
    | MainFunc:
        cf:costFunction
        -> mf:(txSkel:txSkeleton
                -> Zen.Cost.t (result txSkeleton)
                              (Zen.Cost.force ((CostFunc?.f cf) txSkel)))
        -> mainFunction
