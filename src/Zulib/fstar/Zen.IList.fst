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

val ilist_tl_ilist(#a:Type): l:ilist a ->
    Lemma ( match l with
            | [] -> True
            | hd::tl -> is_ilist tl )
let ilist_tl_ilist #_ _ = ()

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

val (::) (#a:Type): a -> ilist a -> ilist a `cost` 2
let (::) #_ x l = cons x l

val cons_length(#a:Type):
    x:a
    -> ls:ilist a
    -> Lemma ( let result = force (cons x ls) in
               length result == length ls + 1 )
let cons_length #_ _ _ = ()

val head(#a:Type): ls:ilist a{length ls >= 1} -> a `cost` 1
let head #_ ((hd,_)::_) = 1 +~! hd

val tryHead(#a:Type): ls:ilist a -> option a `cost` 2
let tryHead #_ = function
    | [] -> 2 +~! None
    | (hd,_)::_ -> 2 +~! Some hd

val tail(#a:Type): ls:ilist a{length ls >= 1} -> ilist a `cost` 1
let tail #_ (_::tl) = 1 +~! tl

val tryTail(#a:Type): ls:ilist a -> option (ilist a) `cost` 2
let tryTail #_ = function
    | [] -> 2 +~! None
    | _::tl -> 2 +~! Some tl

val nth(#a:Type): ls:ilist a -> n:nat{n < length ls} -> a `cost` (4 * n + 4)
let rec nth #_ ls n = match ls with
    | (hd,_)::tl -> if n = 0 then 4 +~! hd else 4 +! nth tl (n-1)

val tryNth(#a:Type): ilist a -> n:nat -> option a `cost` (4 * n + 4)
let rec tryNth #_ ls n = match ls with
    | [] -> (4*n+4) +~! None
    | (hd,_)::tl -> if n = 0 then 4 +~! Some hd else 4 +! tryNth tl (n-1)

val append(#a:Type): l1:ilist a -> l2:ilist a -> ilist a `cost` (4 * length l1 + 4)
let rec append #a l1 l2 = match l1 with
    | [] -> 4 +~! l2
    | (l1_hd,_)::l1_tl ->
        let l1_tl : ilist a = l1_tl in
        let! tl = append l1_tl l2 in
        2 +! cons l1_hd tl

val (++) (#a:Type): l1:ilist a -> l2:ilist a -> ilist a `cost` (4 * length l1 + 4)
let (++) #_ l1 l2 = append l1 l2

val rev_append(#a:Type): l1:ilist a -> l2:ilist a -> ilist a `cost` (4 * length l1 + 4)
let rec rev_append #a l1 l2 = match l1 with
    | [] -> 4 +~! l2
    | (hd,_)::tl ->
        let tl : ilist a = tl in
        let! l2 = 2 +! (cons hd l2) in
        rev_append tl l2

val rev (#a:Type): ls:ilist a -> ilist a `cost` (4 * length ls + 4)
let rev #_ ls = rev_append ls []

// TODO : init
// TODO : flatten
// TODO : concat

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
(*
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
*)
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
