module Zen.Set

open Zen.Cost

assume new type set : eqtype -> eqtype
//assume Set_hasEq: forall (a:eqtype). hasEq (set a)

type t (a:eqtype) = set a

val nullSet (#a:eqtype): set a

val contains(#a:eqtype): a -> set a -> bool `cost` 64

val add(#a:eqtype): a -> set a -> set a `cost` 64

val remove(#a:eqtype): a -> set a -> set a `cost` 64
