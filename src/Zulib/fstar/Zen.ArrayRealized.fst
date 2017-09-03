module Zen.ArrayRealized

open Zen.Cost
module M = FStar.Mul
module V = Zen.Vector

abstract type array (a:Type) : nat -> Type = V.t a

abstract val empty(#a:Type): array a 0
abstract let empty(#_) = V.VNil

(** at returns the element of an array at i. *)
abstract val at(#a:Type)(#l:nat): array a l -> i:nat{i<l} -> cost a 2
abstract let rec at #_ #_ (V.VCons hd tl) = function
  | 0 -> incRet 2 hd
  | i -> at tl (i-1)

let get = at

abstract val ofVec(#a:Type)(#l:nat): V.t a l -> cost (array a l) M.(2*(l+1))
abstract let ofVec #_ #_ v = autoRet v

(** init returns an array of length l for which the i-th element is f(i) *)
assume val init(#a:Type)(#n:nat): l:nat -> f:(i:nat{i<l} -> cost a n)
  -> cost (array a l) M.((l+1)*(n+1))

(** append returns an array containing the elements of arr followed by x. *)
assume val append(#a:Type)(#l:nat): a -> array a l -> cost (array a (l+1)) M.(2*l+2)


(*)
(** sub returns the subrange of arr from i to (i+j). *)
val sub: #a:Type
  -> #l:nat
  -> arr:array a l
  -> i:nat{i<l}
  -> j:nat{j+i <= l}
  -> cost (array a j) ((2*j)+1)

val map: #a:Type
  -> #b:Type
  -> #n:nat
  -> #l:nat
  -> f:(a -> cost b n)
  -> arr:array a l
  -> cost (array b l) (l*(n+1)+1)

(** create returns an array of length n in which each element is x. *)
val create: #a:Type
  -> n:nat
  -> x:a
  -> cost (array a n) (n+1)

(** init returns an array of length l for which the i-th element is f(i) *)
val init: #a:Type
  -> #n:nat
  -> l:nat
  -> f:(nat -> cost a n)
  -> cost (array a l) (n*(l + 1))

(** append returns an array containing the elements of arr followed by x. *)
val append: #a:Type
  -> #l:nat
  -> x:a
  -> arr: array a l
  -> cost (array a (l+1)) (l+2)

(** concat returns an array containing the elements of a1 followed by a2. *)
val concat: #a:Type
  -> #l1:nat
  -> #l2:nat
  -> a1:array a l1
  -> a2:array a l2
  -> cost (array a (l1+l2)) (2 * (l1+l2) + 1)

val concat3: #a:Type
  -> #l1:nat
  -> #l2:nat
  -> #l3:nat
  -> a1:array a l1
  -> a2:array a l2
  -> a3:array a l3
  -> cost (array a (l1+l2+l3)) (2 * (l1+l2+l3) + 1)

(** [split arr i] returns a tuple of the elements of [arr] before [arr.(i)] and the remaining elements. *)
val split: #a:Type
  -> #l:nat{l >= 2}
  -> array a l
  -> i:nat{0 < i /\ i < l}
  -> tuple2 (cost (array a i) (2*i+1)) (cost (array a (l-i)) (2*(l-i)+1))

(** [split3 arr i j] returns [(arr.(0..1), arr.(i..j), arr.(j..))] *)
val split3: #a:Type
  -> #l:nat{l >= 3}
  -> array a l
  -> i:nat{0 < i /\ i < (l-1)}
  -> j:nat{i < j /\ j < l}
  -> tuple3 (cost (array a i)     (2*i+1))
            (cost (array a (j-i)) (2*(j-i)+1))
            (cost (array a (l-j)) (2*(l-j)+1))

(** [split4 arr i j k] returns [(arr.(0..1), arr.(i..j), arr.(j..k), arr.(k..))] *)
val split4: #a:Type
  -> #l:nat{l >= 4}
  -> array a l
  -> i:nat{0 < i /\ i < (l-2)}
  -> j:nat{i < j /\ j < (l-1)}
  -> k:nat{j < k /\ k < l}
  -> tuple4 (cost (array a i)     (2*i+1))
            (cost (array a (j-i)) (2*(j-i)+1))
            (cost (array a (k-j)) (2*(k-j)+1))
            (cost (array a (l-k)) (2*(l-k)+1))
