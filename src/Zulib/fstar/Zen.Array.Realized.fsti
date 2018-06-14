module Zen.Array.Realized

open Zen.Cost

assume new type array (a:Type u#a): Type u#a
assume Array_hasEq: forall (a:Type). {:pattern (hasEq (array a))}
  hasEq a ==> hasEq (array a)

type t (a:Type) = array a

val length(#a:Type): array a -> nat

type indexed (a:Type) (n:nat) = arr:array a{length arr == n}

(** init returns an array of length l for which the i-th element is f(i) *)
val init(#a:Type)(#n:nat):
  l:pos
  -> f:((i:nat{i<l}) -> cost a n)
  -> indexed a l `cost` ((n + 1)*l)

val empty: indexed 'a 0
val init_unique(#a:Type): arr:(a `indexed` 0) -> Lemma (arr == empty)

val item(#a:Type)(#l:nat): i:nat{i<l} -> indexed a l -> cost a 1

val init_item(#a:Type)(#n:nat): l:pos
  -> f:((i:nat{i<l}) -> cost a n)
  -> i:nat{i < l}
  -> Lemma (force (init l f >>= item i) == force (f i))

val tryMap(#a #b:Type)(#n:nat): (a -> cost (option b) n) -> array a
  -> option (array b)