module FStar.String

open Zen.Cost
module Char = FStar.Char
module A = Zen.Array

type t = string

val strlen: string -> nat
let length = strlen

val at: string -> nat -> string

private val strcat: string -> string -> string
val cat: s1:string -> s2:string -> string `cost` (length s1 + length s2 + 2)

(*
val of_chars(#l:nat): chars: Char.t `A.t` n -> s:string{length s = l}
val of_chars_eq(#l:nat): chars: Char.t `A.t` n
    -> Lemma ( forall i:nat)
*)
