module Zen.Array
include Zen.ArrayRealized

open Zen.Cost
module OptT = Zen.OptionT

type t (a:Type) (n:nat) = array a n // For qualified access
assume Array_hasEq: forall (a:Type) (n:nat). {:pattern (hasEq (t a n))}
  hasEq a ==> hasEq (array a n)

unfold let op_Array_Access = at

(** elem returns the i-th element of an array.
  Equivalent to 'at' with the arguments swapped. *)
unfold val elem(#a:Type)(#l:nat): i:nat{i<l} -> array a l -> cost a 2
unfold let elem #_ #_ i arr = arr.(i)

val tryGet(#a:Type)(#l:nat): i:nat -> array a l -> cost (option a) 4
let tryGet #_ #l i arr = if i < l then OptT.incLift 2 arr.(i)
                                  else OptT.incNone 4

(*)
val of_t(#a:Type): a -> cost (array a 1) 2
let of_t #_ x = create 1 x
