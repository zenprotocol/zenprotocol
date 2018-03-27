module Zen.List

open Zen.Base
open Zen.Cost
module OT = Zen.OptionT

val length_cons(#a:Type): hd:a -> tl:list a
    -> Lemma (length (hd::tl) == length tl + 1)
    [SMTPat (hd::tl)]
let rec length_cons #_ _ _ = ()

(* Basic Functions *)

val head(#a:Type): ls:list a{length ls >= 1} -> a
let head #_ (hd::_) = hd

val tail(#a:Type): ls:list a{length ls >= 1} -> list a
let tail #_ (_::tl) = tl

val cons(#a:Type): a -> list a -> list a
let cons #_ hd tl = hd::tl

val (::) (#a:Type): a -> list a -> list a
let (::) #_ = cons

val isNull (#a:Type): list a -> bool
let isNull #_ = Nil?

val append(#a:Type): l1:list a -> l2:list a
    -> list a `cost` (length l1 * 2 + 2)
let rec append #_ l1 l2 = match l1 with
    | [] -> l2 |> incRet 2
    | hd::tl ->
        let! tl = append tl l2 in
        hd::tl |> incRet 2

(* List Transformations *)
val map(#a #b:Type):
    (a -> b)
    -> ls:list a
    -> list b `cost` (length ls * 2 + 2)
let rec map #_ #b f ls = match ls with
    | [] -> [] |> incRet 2
    | hd::tl ->
        let hd = f hd in
        let! tl = f `map` tl in
        hd::tl |> incRet 2

val rev_append(#a:Type): l1:list a -> l2:list a
    -> list a `cost` (2 * length l1 + 2)
let rec rev_append #_ l1 l2 = match l1 with
    | [] -> l2 |> incRet 2
    | hd::tl -> 2 +! rev_append tl (hd::l2)

val rev(#a:Type): ls:list a -> list a `cost` (2 * length ls + 2)
let rev #_ l = rev_append l []

val intersperse(#a:Type): a -> ls:list a -> list a `cost` (4 * length ls + 4)
let rec intersperse #_ x = function
    | [] -> [] |> incRet 4
    | [e] -> [e] |> incRet 8
    | hd::tl ->
        let! tl = intersperse x tl in
        hd::x::tl |> incRet 4

(*)
val nth(#a:Type): ls:list a -> n:nat{n < length ls} -> a `cost` (2 * n + 2)
let rec nth #_ (hd::tl) n = if n = 0 then 2 +~! hd else 2 +! nth tl (n-1)

val tryNth(#a:Type): ls:list a -> n:nat -> option a `cost` (2 * n + 7)
let rec tryNth #_ ls n =
    if n < length ls
    then OT.incLift 5 (nth ls n)
    else OT.incNone (2 * n + 7)

val append(#a:Type): l1:list a -> l2:list a
    -> result : cost (list a) (2 * length l1 + 2)
       { length (force result) = length l1 + length l2 }
let rec append #_ l1 l2 = match l1 with
    | [] -> 2 +~! l2
    | hd::tl ->
        let! tl = append tl l2 in
        2 +~! (hd::tl)

val (++) (#a:Type): l1:list a -> l2:list a -> list a `cost` (2 * length l1 + 2)
let (++) #_ = append

val rev(#a:Type): ls:list a -> list a `cost` (2 * length ls + 2)
let rev #_ l = rev_append l []

val map(#a #b:Type): (a -> b) -> ls:list a -> list b `cost` (2 * length ls + 2)
let rec map #_ #_ f = function
    | [] -> 2 +~! []
    | hd::tl ->
        let hd = f hd in
        let! tl = f `map` tl in
        2 +~! hd::tl

val mapT(#a #b:Type)(#n:nat): (a -> b `cost` n)
    -> ls:list a
    -> list b `cost` ((2 + n) * length ls + 2)
let rec mapT #_ #_ #_ f = function
    | [] -> 2 +~! []
    | hd::tl ->
        let! hd = f hd in
        let! tl = f `mapT` tl in
        2 +~! hd::tl

val mapi_aux(#a #b:Type): nat
    -> (nat -> a -> b)
    -> ls:list a
    -> Tot (result : cost (list b) (2 * length ls + 2)
                     { length (force result) == length ls })
           (decreases (length ls))
let rec mapi_aux #_ #_ n f = function
    | [] -> 2 +~! []
    | hd::tl ->
        let! tl = mapi_aux (n+1) f tl in
        2 +~! ((f n hd)::tl)

val mapi(#a #b:Type): (nat -> a -> b)
    -> ls:list a
    -> result : cost (list b) (2 * length ls + 2)
       { length (force result) == length ls }
let mapi #_ #_ f ls = mapi_aux 0 f ls
