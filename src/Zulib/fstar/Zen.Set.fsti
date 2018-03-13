module Zen.Set

open Zen.Cost

assume new type set (t: eqtype) : eqtype

type t (a:eqtype) = set a

val count(#a:eqtype): set a -> nat

val contains(#a:eqtype): a -> set a -> bool `cost` 64

val add(#a:eqtype): a -> set a -> set a `cost` 64
(* adding an element to a set implies that it is contained in the set *)
val add_contains(#a:eqtype): x:a
    -> s:set a
    -> Lemma ( let s' = force (add x s) in
               force (contains x s') == true )
val add_contains_invariant(#a:eqtype): x:a
    -> y:a
    -> s:set a
    -> Lemma ( let s' = force (add x s) in
               force (contains y s) = true ==> force (contains y s') = true )
val add_count(#a:eqtype): x:a
    -> s:set a
    -> Lemma ( let s' = force (add x s) in
               (force (contains x s) = true <==> count s' = count s)
               /\ (force (contains x s) = false <==> count s' = count s + 1) )

val remove(#a:eqtype): a -> set a -> set a `cost` 64
val remove_contains(#a:eqtype): x:a
    -> s:set a
    -> Lemma ( let s' = force (remove x s) in
               force (contains x s') == false )
val remove_contains_invariant(#a:eqtype): x:a
    -> y:a
    -> s:set a
    -> Lemma ( let s' = force (remove x s) in
               y =!= x ==> force (contains y s) == force (contains y s') )
val remove_count(#a:eqtype): x:a
   -> s:set a
   -> Lemma ( let s' = force (remove x s) in
              (force (contains x s) = true <==> count s' = count s - 1)
              /\ (force (contains x s) = false <==> count s' = count s) )


val empty (#a:eqtype): set a
val empty_contains(#a:eqtype): x:a -> Lemma (force (contains x empty) == false)
val empty_count: (a:eqtype) -> Lemma (count (empty #a) == 0)
val empty_unique(#a:eqtype): s:set a -> Lemma (count s == 0 <==> s == empty)
