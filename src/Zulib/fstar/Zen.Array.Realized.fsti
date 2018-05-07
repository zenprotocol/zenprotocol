module Zen.Array.Realized

open Zen.Cost

assume new type array (a:Type u#a) : nat -> Type u#a
assume Array_hasEq: forall (a:Type) (n:nat). {:pattern (hasEq (array a n))}
  hasEq a ==> hasEq (array a n)

(** init returns an array of length l for which the i-th element is f(i) *)
val init(#a:Type)(#n:nat): l:pos
  -> f:((i:nat{i<l}) -> cost a n)
  -> array a l `cost` ((n + 1)*l)

val empty: array 'a 0
val init_unique(#a:Type): arr:(a `array` 0) -> Lemma (arr == empty)


val item(#a:Type)(#l:nat): i:nat{i<l} -> array a l -> cost a 1

val init_item(#a:Type)(#n:nat): l:pos
  -> f:((i:nat{i<l}) -> cost a n)
  -> i:nat{i < l}
  -> Lemma (force (init l f >>= item i) == force (f i))
