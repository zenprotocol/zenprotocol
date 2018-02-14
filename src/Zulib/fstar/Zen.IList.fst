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
val length(#a:Type): ilist a -> nat
let length(#a:Type) = function
    | [] -> 0
    | (_,n)::_ -> n


val length_tl(#a:Type):
    l:ilist a{Cons? l}
    -> Lemma ( let tl = Cons?.tl l in
               length l == length tl + 1)
let length_tl #_ _ = ()

val cons(#a:Type): a -> ilist a -> ilist a `cost` 2
let cons #_ x l =
    match l with
    | [] -> 2 +~! [x,1]
    | (_,n)::_ -> 2 +~! (x, n+1)::l

val cons_length(#a:Type):
    x:a
    -> ls:ilist a
    -> Lemma ( let result = force (cons x ls) in
               length result == length ls + 1 )
let cons_length #_ _ _ = ()

val map(#a #b:Type):
    (a -> b)
    -> ls:ilist a
    -> ilist b `cost` ( length ls * 3 + 3 )
let rec map #a #_ f = function
    | [] -> 3 +~! []
    | (hd, _)::tl ->
        let tl: ilist a = tl in
        let! tl = f `map` tl in
        1 +! cons (f hd) tl

val map_length(#a #b:Type):
    f:(a -> b)
    -> ls:ilist a
    -> Lemma ( let result = force (f `map` ls) in
               length result = length ls )
let rec map_length #_ #_ f = function
    | [] -> ()
    | _::tl -> map_length f tl

val mapT(#a #b:Type)(#n:nat):
    (a -> b `cost` n)
    -> ls:ilist a
    -> ilist b `cost` ((3 + n) * length ls + 3)
let rec mapT #a #_ #_ f = function
    | [] -> 3 +~! []
    | (hd,_)::tl ->
        let tl: ilist a = tl in
        let tl = f `mapT` tl in
        1 +! (bind2 (f hd) tl cons)

val mapT_cons(#a #b:Type)(#n:nat):
    f:(a -> b `cost` n)
    -> ls:ilist a
    -> Lemma ( match ls with
               | [] -> True
               | (hd,_)::tl ->
                    let hd = force (f hd) in
                    let tl: ilist a = tl in
                    let tl = force (f `mapT` tl) in
                    force (f `mapT` ls) == force (cons hd tl) )
let mapT_cons #a #_ #_ f ls = ()

val mapT_length(#a #b:Type)(#n:nat):
    f:(a -> b `cost` n)
    -> ls:ilist a
    -> Lemma ( let result = force (f `mapT` ls) in
               length result = length ls )
let rec mapT_length #a #_ #_ f ls =
    match ls with
    | [] -> ()
    | (hd,_)::tl ->
        let tl: ilist a = tl in
        mapT_length f tl;
        mapT_cons f ls

val foldl(#a #b:Type):
    (a -> b -> a)
    -> a
    -> ls:ilist b
    -> Tot (a `cost` ( length ls * 3 + 3 ))
    (decreases (length ls))
let rec foldl #_ #b f acc = function
    | [] -> 3 +~! acc
    | (hd,_)::tl ->
        let acc = f acc hd in
        let tl : ilist b = tl in
        3 +! foldl f acc tl

val foldlT(#a #b:Type)(#n:nat):
    (a -> b -> a `cost` n)
    -> a
    -> ls:ilist b
    -> Tot (a `cost` ( (3 + n) * length ls + 3 ))
    (decreases (length ls))
let rec foldlT #_ #b #_ f acc = function
    | [] -> 3 +~! acc
    | (hd,_)::tl ->
        let tl : ilist b = tl in
        let! acc = f acc hd in
        3 +! foldlT f acc tl

val sumBy(#a:Type):
    (a -> int)
    -> ls:ilist a
    -> int `cost` (3 * length ls + 3 )
let sumBy #_ f =
    foldl (fun acc x -> f x + acc) 0

val sumByT(#a:Type)(#n:nat):
    (a -> int `cost` n)
    -> ls:ilist a
    -> int `cost` ((3 + n) * length ls + 3 )
let sumByT #_ #n f =
    foldlT (fun acc x -> (+) acc `Cost.map` f x) 0
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
