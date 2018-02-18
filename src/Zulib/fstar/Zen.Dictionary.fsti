module Zen.Dictionary

open Zen.Cost

(* A dictionary is a map from `string` to some other type. *)
assume new type dictionary (t:Type u#a) : Type u#a
assume Dictionary_hasEq: forall (a:Type). hasEq a ==> hasEq (dictionary a)
// for qualified imports
type t (a:Type) = dictionary a

(* Returns the number of elements in the dictionary *)
val size(#a:Type): dictionary a -> nat

(* Returns `true` if the dictionary contains the given key. *)
val containsKey(#a:Type): key:string -> dictionary a -> bool `cost` 64
(*
// if a dictionary contains a key k, then it's size is at least 1,
// and if a dictionary has a size of at least 1, then it must contain a key.
assume ContainsSize: forall (a:Type) (d:dictionary a).
    (exists (k:string). force (containsKey k d) = true) <==> size d >= 1
*)

(* Returns the value with key `k`, if it exists. *)
val tryFind(#a:Type): k:string -> dictionary a -> option a `cost` 64
(*
assume TryFindContainsKey: forall (a:Type) (d:dictionary a) (k:string).
    let result = force (tryFind k d) in
    let d_contains_k = force (containsKey k d) in
    (d_contains_k = true <==> Some? result)
    /\
    (d_contains_k = false <==> None? result)
*)

(* Adds `value` to a dictionary with key `k`. *)
val add(#a:Type): k:string -> value:a -> dictionary a -> dictionary a `cost` 64
(*
// adding a value v with key k to a dictionary:
//     increases it's size by 1 if it does not contain the key
//     does not change it's size if it does contain the key
assume AddSize: forall (a:Type) (d:dictionary a) (k:string) (v:a).
    let result = force (add k v d) in
    let d_contains_k = force (containsKey k d) in
    (d_contains_k = true <==> size result == size d)
    /\
    (d_contains_k = false <==> size result == size d + 1)
// if a key exists in a dictionary, then it also exists after an add.
assume AddKey: forall (a:Type) (d:dictionary a) (k:string) (v:a).
    let result = force (add k v d) in
    let d_contains_k = force (containsKey k d) in
    let result_contains_k = force (containsKey k result) in
    d_contains_k = true ==> result_contains_k = true
// if a dictionary does not contain the key k2, then for a key k1 =/= k2,
// k2 does not exist after adding k1.
assume AddKeyNegative: forall (a:Type) (d:dictionary a) (k1 k2:string) (v:a).
    let result = force (add k1 v d) in
    let d_contains_k2 = force (containsKey k2 d) in
    let result_contains_k2 = force (containsKey k2 result) in
    ((k1 <> k2) /\ d_contains_k2 = false) <==> result_contains_k2 = false
// the value at k1 after adding v add at k1 is v.
// the value of k2 after adding v at k1 is the value of k2 in d, if k1 =/= k2.
assume AddTryFind: forall (a:Type) (d:dictionary a) (k1 k2:string) (v:a).
    let result = force (add k1 v d) in
    let d_k2 = force (tryFind k2 d) in
    let result_k1 = force (tryFind k1 result) in
    let result_k2 = force (tryFind k2 result) in
    (result_k1 == Some v)
    /\
    ((k1 <> k2) ==> result_k2 == d_k2)
*)

(* Removes a key-value pair from a dictionary,
   if the key `k` exists in the dictionary. *)
val remove(#a:Type): k:string -> dictionary a -> dictionary a `cost` 64
(*
// removing a key from a dictionary:
//     decreases it's size by 1 if it contains the key
//     does not change it's size if it does not contain the key
assume RemoveSize: forall (a:Type) (d:dictionary a) (k:string).
    let result = force (remove k d) in
    let d_contains_k = force (containsKey k d) in
    (d_contains_k = true <==> size result == size d - 1)
    /\
    (d_contains_k = false <==> size result == size d)
// if a key exists in a dictionary, then it does not exist after a removal.
assume RemoveKey: forall (a:Type) (d:dictionary a) (k:string).
    let result = force (remove k d) in
    force (containsKey k result) = false
// if a dictionary contains a key k2, and k2 =/= k1,
// then k2 exists after a removal of k1.
assume RemoveKeyNegative: forall (a:Type) (d:dictionary a) (k1 k2:string).
    let result = force (remove k1 d) in
    let d_contains_k2 = force (containsKey k2 d) in
    let result_contains_k2 = force (containsKey k2 result) in
    ((k1 <> k2) /\ d_contains_k2 = true) <==> result_contains_k2 = true
// the value at k1 after removing k1 is None.
// the value of k2 after removing k1 is the value of k2 in d, if k1 =/= k2.
assume AddTryFind: forall (a:Type) (d:dictionary a) (k1 k2:string).
    let result = force (remove k1 d) in
    let d_k2 = force (tryFind k2 d) in
    let result_k1 = force (tryFind k1 result) in
    let result_k2 = force (tryFind k2 result) in
    (result_k1 == None)
    /\
    ((k1 <> k2) ==> result_k2 == d_k2)
*)

(* an empty dictionary *)
val empty(#a:Type): dictionary a
(*
// Empty dictionary has 0 size
assume EmptySize: forall (a:Type). size (empty #a) == 0
// Empty dictionary is the only dictionary with size 0
assume EmptyUnique: forall (a:Type) (d:dictionary a).
    size d = 0 <==> d == empty
// Empty dictionary does not contain any key.
assume EmptyContainsKey: forall (a:Type) (key:string).
    force (containsKey key (empty #a)) == false
*)
