module Zen.Vector

open Zen.Base
open Zen.Cost

type vector(a:Type): nat -> Type =
  | VNil : vector a 0
  | VCons : #l:nat
    -> hd:a
    -> tl:vector a l
    -> vector a (l+1)

type t(a:Type): nat -> Type = vector a // For qualified imports

(** [isEmpty v] returns [true] if and only if [v] is empty. *)
val isEmpty(#a:Type)(#l:nat): vector a l -> cost bool 3
let isEmpty #_ #_ = function | VNil      -> 3 `incRet` true
                             | VCons _ _ -> 3 `incRet` false

(** [hd v] returns the first element of [v].
    Requires [v] to be nonempty.*)
val hd(#a:Type)(#l:nat): v:vector a l{VCons? v} -> cost a 2
let hd #_ #_ (VCons hd _) = 2 `incRet` hd

(** [tl v] returns the tail of [v].
    Requires [v] to be nonempty.*)
val tl(#a:Type)(#l:nat): v:vector a l{VCons? v} -> cost (vector a (l-1)) 2
let tl #_ #_ (VCons _ tl) = 2 `incRet` tl

(** [nth v i] returns the [i]th element of v, starting at 0. *)
val nth(#a:Type)(#l:nat): vector a l -> i:nat{i<l} -> cost a (4*(i+1))
let rec nth #_ #_ (VCons hd tl) = function
  | 0 -> hd |> incRet 4
  | i -> nth tl (i-1) |> inc 4

val rev_append(#a:Type)(#l1 #l2:nat): vector a l1 -> vector a l2
    -> Tot (vector a (l1+l2) `cost` (l1 * 4 + 4))
           (decreases l1)
let rec rev_append #_ #_ #_ v1 v2 = match v1 with
    | VNil -> v2 |> incRet 4
    | VCons hd tl ->
        rev_append tl (VCons hd v2)
        |> inc 4

(** [append v1 v2] appends the elements of [v2] to the end of [v1]. *)
val append(#a:Type)(#l1 #l2:nat):
  vector a l1 -> vector a l2 -> cost (vector a (l1+l2)) (4*l1+4)
let rec append #a #l1 #l2 v1 v2 =
  begin match v1 with
  | VCons hd tl ->
      (let! tl = append tl v2 in
      ret (VCons hd tl)) <: cost (vector a (l1+l2)) (4*l1)
  | VNil -> ret v2
  end
  |> inc 4

unfold val (++) (#a:Type)(#l1 #l2:nat):
  vector a l1 -> vector a l2 -> cost (vector a (l1+l2)) (4*l1+4)
unfold let (++) #_ #_ #_ v1 v2 = append v1 v2

(** [init l f] returns a vector of length l, for which the [i]th element is [f i]. *)
val init(#a:Type)(#n:nat): l:nat -> (i:nat{i<l} -> cost a n)
  -> cost (vector a l) (n*l+2)
let rec init #_ #_ l f = match l with
  | 0 -> 2 `incRet` VNil
  | _ -> let l':nat = l-1 in
         VCons <$> (f 0) <*> (init l' (fun x -> f (x+1)))

(** [map f v] returns a vector of length l,
    for which the [i]th element is the result of f applied to the [i]th element of v.*)
val map(#a #b:Type)(#l #n:nat): (a -> cost b n) -> vector a l
  -> cost (vector b l) ((l*n)+(l*2)+2)
let rec map #_ #b #l #n f = function
  | VNil -> 2 `incRet` VNil
  | VCons hd tl ->
      VCons <$> (f hd) <*> (map f tl)
      |> inc 2

val foldl(#a #b:Type)(#l #n:nat):
  (a -> b -> cost a n) -> a -> vector b l
  -> cost a ((n+2)*l+2)
let rec foldl #_ #_ #_ #_ f acc = function
  | VNil -> 2 `incRet` acc
  | VCons hd tl ->
      f acc hd
      >>= (fun acc' -> foldl f acc' tl)
      |> inc 2

(** [countWhere f v] returns the number of elements [e] in [v] for which [f e] is true. *)
val countWhere(#a:Type)(#l #n:nat): (a -> cost bool n) -> vector a l
  -> res: cost nat ((n+2)*l+2){force res <= l}
let countWhere #_ #_ #_ f =
  let ctr (acc:nat) hd = f hd $> (fun b -> if b then acc+1 <: nat else acc) in
  admit();
  foldl ctr 0

val forAll(#a:Type)(#l #n:nat):
    (a -> bool `cost` n)
    -> vector a l
    -> bool `cost` ((n+2)*l+2)
let forAll #_ #_ #_ f =
    let aux acc x  =
        let! fx = f x in
        if acc && fx = true
        then ret true
        else ret false
    in
    foldl aux true

val sumBy(#a:Type)(#l #n:nat):
    (a -> int `cost` n)
    -> vector a l
    -> int `cost` ((n+2)*l+2)
let sumBy #_ #_ #_ f =
    let aux acc x =
        let! fx = f x in
        ret (fx + acc)
    in
    foldl aux 0

val sumZ(#l:nat):
    vector int l
    -> int `cost` (2*l+2)
let sumZ #_ =
    sumBy ret

val zip(#a #b:Type)(#l:nat):
  vector a l -> vector b l -> cost (vector (a**b) l) (3*l+3)
let rec zip #_ #_ #_ v1 v2 =
  match v1 with
  | VNil -> 3 `incRet` VNil
  | VCons hd1 tl1 ->
      match v2 with
      | VCons hd2 tl2 ->
          VCons (hd1,hd2) <$> (zip tl1 tl2)
          |> inc 3

val of2(#a:Type): a**a -> cost (vector a 2) 3
let of2 #_ (x,y) = incRet 3 (VCons x (VCons y VNil))

val of3(#a:Type): a**a**a -> cost (vector a 3) 4
let of3 #_ (x,y,z) = incRet 4 (VCons x (VCons y (VCons z VNil)))
