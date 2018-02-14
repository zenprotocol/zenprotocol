module Zen.IList

// TODO: make this module more accurately recflect the elaborator cost model
open Zen.Base
open Zen.Cost

noextract val is_ilist(#a:Type): list (a ** nat) -> Type0
noextract let rec is_ilist #_ = function
    | [] -> True
    | (_, m)::tl ->
        begin match tl with
        | [] -> m == 1
        | (_,n)::_ -> is_ilist tl /\ m == n + 1
        end

(* An `ilist a` is a `list a` which is indexed by it's length. *)
type ilist (a:Type) = l:list (a ** nat){ is_ilist l}

// for qualified import
type t (a:Type) = ilist a

val ilist_tl(#a:Type): l:ilist a ->
    Lemma ( match l with
            | [] -> True
            | hd::tl -> is_ilist tl )
let ilist_tl #_ _ = ()

(* gets the length of an ilist *)
val length(#a:Type): ilist a -> nat `cost` 2
let length(#a:Type) = function
    | [] -> 2 +~! 0
    | (_,n)::_ -> 2 +~! n


val length_tl(#a:Type):
    l:ilist a{Cons? l}
    -> Lemma ( let tl = Cons?.tl l in
               let length_l  = length l  |> force in
               let length_tl = length tl |> force in
               length_l == length_tl + 1
             )
let length_tl #_ _ = ()

val cons(#a:Type): a -> ilist a -> ilist a `cost` 2
let cons #_ x l =
    match l with
    | [] -> 2 +~! [x,1]
    | (_,n)::_ -> 2 +~! (x, n+1)::l

val cons_length(#a:Type):
    x:a
    -> ls:ilist a
    -> Lemma ( let length_l = force (length ls) in
               let length_result = force (length =<< cons x ls) in
               length_result == length_l + 1 )
let cons_length #_ _ _ = ()

val map(#a #b:Type):
    (a -> b)
    -> ls:ilist a
    -> ilist b `cost` ( let length_ls = match ls with
                                        | [] -> 0
                                        | (_, n)::_ -> n in
                        length_ls * 3 + 3 )
let rec map #a #b f l =
    match l with
    | [] -> 3 +~! []
    | (hd, n)::tl ->
        let tl: ilist a = tl in
        let! tl = f `map` tl in
        1 +! cons (f hd) tl

val map_length(#a #b:Type):
    f:(a -> b)
    -> ls:ilist a
    -> Lemma ( let length_ls = force (length ls) in
               let length_result = force (length =<< map f ls) in
               length_result = length_ls )
let rec map_length #_ #_ f = function
    | [] -> ()
    | _::tl -> map_length f tl

(*)
val test_cons: ls:cost (ilist bool) 12
    { let l = force (length =<< ls) in
      l = 6 }
let test_cons =
        cons true []
        >>= cons true
        >>= cons false
        >>= cons true
        >>= cons true
        >>= cons false
(*)
val test_imap: ls:


(*)
type ilist' (a:Type) =
    | INil : ilist' a
    | ICons : hd:a -> tl:ilist' a -> n:nat -> ilist' a

private val is_ilist(#a:Type): ilist' a -> Type0
private let rec is_ilist #_ = function
    | INil -> True
    | ICons hd tl n ->
        is_ilist tl /\
        begin match tl with
        | INil -> n == 1
        | ICons _ _ n' -> n == n' + 1
        end

type ilist (a:Type) = l:ilist' a{is_ilist l}


val tl_ilist(#a:Type): l:ilist a
    -> Lemma ( match l with
               | INil -> True
               | ICons _ tl _ -> is_ilist tl )
let tl_ilist #_ _ = ()

val iCons(#a:Type): a -> ilist a -> ilist a
let iCons #_ x l =
    match l with
    | INil -> ICons x INil 1
    | ICons _ _ n -> ICons x l (n+1)

val ilength(#a:Type): ilist a -> nat
let ilength(#a:Type) = function
    | INil -> 0
    | ICons _ _ n -> n

val imap(#a #b:Type): (a -> b) -> l:ilist a -> ilist b `cost` ilength l
let rec imap #a #_ f l =
    match l with
    | INil -> ret INil
    | ICons hd tl n ->
        let tl = tl <: ilist a in
        let! ftl = f `imap` tl in
        1 +~! iCons (f hd) ftl n
