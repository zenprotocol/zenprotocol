module Zen.Array.Extracted

open Zen.Base
open Zen.Cost
open Zen.Array.Realized
//module OptT = Zen.OptionT
module M = FStar.Mul
module V = Zen.Vector

type t (a:Type) (n:nat) = array a n // For qualified access

unfold val at(#a:Type)(#l:nat): array a l -> i:nat{i<l} -> cost a 1
unfold let at #_ #_ arr i = elem i arr

unfold val get(#a:Type)(#l:nat): array a l -> i:nat{i<l} -> cost a 1
unfold let get = at

unfold val op_Array_Access (#a:Type)(#l:nat): array a l -> i:nat{i<l} -> cost a 1
unfold let op_Array_Access #_ #_ arr i = arr `at` i

(** create returns an array of length l in which each element is x. *)
val create(#a:Type): l:nat -> x:a -> cost (array a l) (l+1)
let create(#_) l x = init l (const_ x >> ret)

val toVec(#a:Type)(#l:nat): array a l -> cost (V.t a l) M.(l+2)
let toVec #_ #l arr = V.init l (get arr)

(** sub returns the subrange of arr from i to (i+j). *)
val sub(#a:Type)(#l:nat): arr:array a l -> i:nat{i<l} -> j:nat{j+i <= l}
  -> cost (array a j) M.((j+1)*2)
let sub #_ #_ arr i j = init j (fun idx -> arr.(idx+i))

(** Creates an array whose elements are the results of applying the supplied function to each of the elements of a supplied array. *)
val map(#a #b:Type)(#n #l:nat): (a -> cost b n) -> array a l
  -> cost (array b l) M.((l+1)*(n+2))
let map #_ #_ #_ #l f arr = init l (get arr >=> f)

(** [concat a1 a2] returns an array containing the elements of a1 followed by the elements of a2. *)
val concat(#a:Type)(#l1 #l2:nat): array a l1 -> array a l2
  -> cost (array a (l1+l2)) M.((l1+l2+1)*2)
let concat #_ #l1 #l2 a1 a2 =
  let fetch (i:nat{ i < (l1+l2)}) = if i < l1 then a1.(i) else a2.(i-l1) in
  init (l1+l2) fetch

(** [concat3 a1 a2] returns an array containing the elements of a1 followed by the elements of a2 followed by the elements of a3. *)
val concat3(#a:Type)(#l1 #l2 #l3:nat): array a l1 -> array a l2 -> array a l3
  -> cost (array a (l1+l2+l3)) M.((l1+l2+l3+1)*2)
let concat3 #_ #l1 #l2 #l3 a1 a2 a3 =
  let fetch (i:nat{ i < (l1+l2+l3)}) = if i < l1      then a1.(i)
                                  else if i < l1 + l2 then a2.(i-l1)
                                  else                     a3.(i-l1-l2) in
  init (l1+l2+l3) fetch

(** [split arr i] returns a tuple of the elements of [arr] before [arr.(i)], and the remaining elements. *)
val split(#a:Type)(#l:nat{l >= 2}): arr:array a l -> i:nat{0 < i /\ i < l}
  -> cost (array a i * array a (l-i)) M.(2*(l+2))
let split #_ #l arr i = Zen.TupleT.join (sub arr 0 i, sub arr i (l-i))

(** [split3 arr i j] returns [(arr.(0..i), arr.(i..j), arr.(j..))] *)
val split3(#a:Type)(#l:nat{l >= 3}):
  arr: array a l
  -> i:nat{0 < i /\ i < (l-1)}
  -> j:nat{i < j /\ j < l}
  -> cost (array a i * array a (j-i) * array a (l-j)) M.(2*(l+3))
let split3 #_ #l arr i j = Zen.TupleT.join3 (sub arr 0 i
                                           , sub arr i (j-i)
                                           , sub arr j (l-j))

(** [split4 arr i j] returns [(arr.(0..i), arr.(i..j), arr.(j..k), arr.(j..))] *)
val split4(#a:Type)(#l:nat{l >= 4}):
 arr: array a l
 -> i:nat{0 < i /\ i < (l-2)}
 -> j:nat{i < j /\ j < (l-1)}
 -> k:nat{j < k /\ k < l}
 -> cost (array a i * array a (j-i) * array a (k-j) * array a (l-k)) M.(2*(l+4))
let split4 #_ #l arr i j k = Zen.TupleT.join4 (sub arr 0 i
                                          , sub arr i (j-i)
                                          , sub arr j (k-j)
                                          , sub arr k (l-k))
