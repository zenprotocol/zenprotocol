module Zen.Dictionary

open Zen.Cost

(* A dictionary is a map from `string` to some other type. *)
assume new type dictionary (t:Type u#a) : Type u#a
assume Dictionary_hasEq: forall (a:eqtype). hasEq (dictionary a)

type t (a:Type) = dictionary a

val empty(#a:Type): dictionary a

val add(#a:Type): string -> a -> dictionary a -> dictionary a `cost` 64

val containsKey(#a:Type): string -> dictionary a -> bool `cost` 64

val remove(#a:Type): string -> dictionary a -> dictionary a `cost` 64

val tryFind(#a:Type): string -> dictionary a -> option a `cost` 64
