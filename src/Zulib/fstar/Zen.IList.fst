module Zen.IList

// TODO: make this module more accurately recflect the elaborator cost model
//open Zen.Base
open Zen.Cost

private val is_ilist(#a:Type): list (a ** nat) -> Type0
private let rec is_ilist #_ = function
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
    -> Lemma ( let length_l  = force (length l) in
               let length_tl = force (length (Cons?.tl l)) in
               length_l == length_tl + 1
             )
let length_tl #_ _ = ()


val cons(#a:Type): a -> ilist a -> ilist a `cost` 2
let cons #_ x l =
    match l with
    | [] -> 2 +~! [x,1]
    | (_,n)::_ -> 2 +~! (x, n+1)::l

//let i = cons true [false;true;false;true]
val i: i:(cost (ilist bool) 12){let l = i |> force in True}
let i =
        cons true []
        >>= cons true
        >>= cons false
        >>= cons true
        >>= cons true
        >>= cons false
(*)
val iCons_length(#a:Type): x:a -> l:ilist a ->
    Lemma (ilength (iCons x l) == ilength l + 1)
let iCons_length #_ _ _ = ()

val imap(#a #b:Type):
    (a -> b)
    -> l:ilist a
    -> ilist b `cost` ilength l
let rec imap #a #b f l =
    match l with
    | [] -> ret []
    | (hd, n)::tl ->
        let tl: ilist a = tl in
        let! tl = f `imap` tl in
        1 +~! iCons (f hd) tl

val imap_length(#a #b:Type):
    f:(a -> b)
    -> l:ilist a
    -> Lemma ( let l' = f `imap` l in
               ilength (force l') = ilength l)
let rec imap_length #_ #_ f = function
    | [] -> ()
    | hd::tl -> imap_length f tl
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
