module Zen.Array.Realized

open Zen.Cost
module V = Zen.Vector

assume new type array (a:Type u#a) : nat -> Type u#a
assume Array_hasEq: forall (a:Type) (n:nat). {:pattern (hasEq (array a n))}
  hasEq a ==> hasEq (array a n)

(** init returns an array of length l for which the i-th element is f(i) *)
val init(#a:Type)(#n:nat): l:nat -> f:(i:nat{i<l} -> cost a n)
  -> cost (array a l) ((l+1)*(n+1))

val empty: array 'a 0

val elem(#a:Type)(#l:nat): i:nat{i<l} -> array a l -> cost a 1

assume Init_elem:
  forall #a #n (l:nat) (f: i:nat{i<l} -> cost a n) (i:nat{i<l}).
  force (init l f >>= elem i) == force (f i)

val ofVec(#a:Type)(#l:nat): V.t a l -> cost (array a l) (2*(l+1))
assume OfVec_elem:
  forall #a #l (v:V.t a l) (i:nat{i<l}).
  force (ofVec v >>= elem i) == force (V.nth v i)
