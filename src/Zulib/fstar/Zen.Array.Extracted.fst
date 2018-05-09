module Zen.Array.Extracted

open Zen.Base
open Zen.Cost
open Zen.Array.Realized

unfold val get(#a:Type)(#l:nat): indexed a l -> i:nat{i<l} -> cost a 1
unfold let get #_ #_ arr i = item i arr

unfold val at(#a:Type)(#l:nat): indexed a l -> i:nat{i<l} -> cost a 1
unfold let at #_ #_ arr i = item i arr

unfold val op_Array_Access (#a:Type)(#l:nat): indexed a l -> i:nat{i<l} -> cost a 1
unfold let op_Array_Access #_ #_ arr i = item i arr

(** create returns an array of length l in which each element is x. *)
val create(#a:Type): l:pos -> x:a -> cost (indexed a l) l
let create(#_) l x = init l (const_ x >> ret)

(** sub returns the subrange of arr from i to (i+j). *)
val sub(#a:Type)(#l:pos): arr:indexed a l -> i:nat{i<l} -> j:pos{j+i <= l}
  -> cost (indexed a j) (j * 2)
let sub #_ #_ arr i j = init j (fun idx -> arr.(idx+i))

(** Creates an array whose elements are the results of applying the supplied function to each of the elements of a supplied array. *)
val map(#a #b:Type)(#n #l:nat): (a -> cost b n) -> indexed a l
  -> cost (indexed b l) ((n+2)*l)
let map #_ #_ #_ #l f arr =
    if l = 0 then ret empty else
    init l (get arr >=> f)

(** [concat a1 a2] returns an array containing the elements of a1 followed by the elements of a2. *)
val concat(#a:Type)(#l1 #l2:nat): indexed a l1 -> indexed a l2
  -> cost (indexed a (l1+l2)) (2*(l1+l2))
let concat #a #l1 #l2 a1 a2 =
  let fetch (i:nat{ i < (l1+l2)}): a `cost` 1 =
    if i < l1 then a1.(i) else a2.(i-l1)
  in
  if l1 + l2 = 0 then ret empty else
  init (l1+l2) fetch

(** [concat3 a1 a2 a3] returns an array containing the elements of a1 followed by the elements of a2 followed by the elements of a3. *)
val concat3(#a:Type)(#l1 #l2 #l3:nat): indexed a l1 -> indexed a l2 -> indexed a l3
  -> cost (indexed a (l1+l2+l3)) (2 * (l1+l2+l3))
let concat3 #_ #l1 #l2 #l3 a1 a2 a3 =
  let fetch (i:nat{ i < (l1+l2+l3)}) = if i < l1      then a1.(i)
                                  else if i < l1 + l2 then a2.(i-l1)
                                  else                     a3.(i-l1-l2)
  in
  if (l1+l2+l3) = 0 then ret empty else
  init (l1+l2+l3) fetch

val chunkBySize(#a:Type)(#l:nat):
    size:pos
    -> a `indexed` l
    -> indexed (indexed a size) (l / size)
       `cost`
       ((l/size)*(2*size+1))
let chunkBySize #a #l size arr =
    let chunk (i: nat{i < (l / size)})
              : indexed a size `cost` (2*size) =
              assert(i <= (l/size) - 1);
              sub arr (i * size) size
    in
    if l / size = 0 then ret empty else
    init (l / size) chunk

(** [split arr i] returns a tuple of the elements of [arr] before [arr.(i)], and the remaining elements. *)
val split(#a:Type)(#l:nat{l >= 2}): arr:indexed a l -> i:nat{0 < i /\ i < l}
  -> cost (indexed a i ** indexed a (l-i)) (2*l)
let split #_ #l arr i =
    let fst, snd = sub arr 0 i, sub arr i (l-i) in
    Mktuple2 <$> fst <*> snd

(** [split3 arr i j] returns [(arr.(0..i), arr.(i..j), arr.(j..))] *)
val split3(#a:Type)(#l:nat{l >= 3}):
  arr: indexed a l
  -> i:nat{0 < i /\ i < (l-1)}
  -> j:nat{i < j /\ j < l}
  -> cost (indexed a i ** indexed a (j-i) ** indexed a (l-j)) (2*l)
let split3 #_ #l arr i j =
    let fst, snd, third = sub arr 0 i, sub arr i (j-i), sub arr j (l-j) in
    Mktuple3 <$> fst <*> snd <*> third

(** [split4 arr i j] returns [(arr.(0..i), arr.(i..j), arr.(j..k), arr.(j..))] *)
val split4(#a:Type)(#l:nat{l >= 4}):
  arr: indexed a l
  -> i:nat{0 < i /\ i < (l-2)}
  -> j:nat{i < j /\ j < (l-1)}
  -> k:nat{j < k /\ k < l}
  -> cost (indexed a i ** indexed a (j-i) ** indexed a (k-j) ** indexed a (l-k)) (2*l)
let split4 #_ #l arr i j k =
    let fst, snd, third, fourth =
        sub arr 0 i, sub arr i (j-i), sub arr j (k-j), sub arr k (l-k) in
    Mktuple4 <$> fst <*> snd <*> third <*> fourth
