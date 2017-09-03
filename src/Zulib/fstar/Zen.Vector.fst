module Zen.Vector

open Zen.Cost
module M = FStar.Mul

type vector(a:Type): nat -> Type =
  | VNil : vector a 0
  | VCons : #l:nat
    -> hd:a
    -> tl:vector a l
    -> vector a (l+1)
type t(a:Type): nat -> Type = vector a // For qualified imports

(** [isEmpty v] returns [true] if and only if [v] is empty. *)
val isEmpty(#a:Type)(#l:nat): vector a l -> cost bool 3
let isEmpty #_ #_ = function | VNil      -> 3 +~! true
                             | VCons _ _ -> 3 +~! false

(** [hd v] returns the first element of [v].
    Requires [v] to be nonempty.*)
val hd(#a:Type)(#l:nat): v:vector a l{VCons? v} -> cost a 2
let hd #_ #_ (VCons hd _) = 2 +~! hd

(** [tl v] returns the first element of [v].
    Requires [v] to be nonempty.*)
val tl(#a:Type)(#l:nat): v:vector a l{VCons? v} -> cost (vector a (l-1)) 2
let tl #_ #_ (VCons _ tl) = 2 +~! tl

(** [nth v i] returns the [i]th element of v, starting at 0. *)
val nth(#a:Type)(#l:nat): vector a l -> i:nat{i<l} -> cost a M.(4*(i+1))
let rec nth #_ #_ (VCons hd tl) = function
  | 0 -> 4 +~! hd
  | i -> 4 +! (nth tl (i-1))

(** [append v1 v2] appends the elements of [v2] to the end of [v1]. *)
val append(#a:Type)(#l1 #l2:nat):
  vector a l1 -> vector a l2 -> cost (vector a (l1+l2)) M.(4*(l1+1))
let rec append #_ #_ #_ v1 v2 = match v1 with
  | VNil -> 4 +~! v2
  | VCons hd tl -> 4 +! (VCons hd <$> (append tl v2))

let (@@) = append

(** [flatten v], where [v] is a vector of vectors of constant length,
    returns the vector of elements of vectors in [v], preserving their order. *)
val flatten(#a:Type)(#l1 #l2:nat):
  vector (vector a l1) l2 -> cost (vector a M.(l1*l2)) M.(4*(l1+1)*(l2+1))
let rec flatten #_ #_ #_ = function
  | VNil -> autoRet VNil
  | VCons hd tl -> append hd =<< (flatten tl)

(** [init l f] returns a vector of length l, for which the [i]th element is [f i]. *)
val init(#a:Type)(#n:nat): l:nat -> (nat -> cost a n)
  -> cost (vector a l) M.(n*l+2)
let rec init #_ #_ l f = match l with
  | 0 -> 2 +~! VNil
  | _ -> let l':nat = l-1 in
         VCons <$> (f 0) <*> (init l' (fun x -> f (x+1)))

(** [map f v] returns a vector of length l,
    for which the [i]th element is the result of f applied to the [i]th element of v.*)
val map(#a #b:Type)(#l #n:nat): (a -> cost b n) -> vector a l
  -> cost (vector b l) M.((l*n)+(l*2)+2)
let rec map #_ #b #l #n f = function
 | VNil -> 2 +~! VNil
 | VCons hd tl -> 2 +! (VCons <$> (f hd) <*> (map f tl))

val foldl(#a #b:Type)(#l #n:nat):
  (a -> b -> cost a n) -> a -> vector b l
  -> cost a M.((n+2)*l+2)
let rec foldl #_ #_ #_ #_ f acc = function
  | VNil -> 2 +~! acc
  | VCons hd tl -> 2 +! (f acc hd >>= (fun acc' -> foldl f acc' tl))

(** [countWhere f v] returns the number of elements [e] in [v] for which [f e] is true. *)
val countWhere(#a:Type)(#l #n:nat): (a -> cost bool n) -> vector a l
  -> res: cost nat M.((n+2)*l+2){force res <= l}
let countWhere #_ #_ #_ f =
  let ctr (acc:nat) hd = f hd $> (fun b -> if b then acc+1 <: nat else acc) in
  admit();
  foldl ctr 0


val zip(#a #b:Type)(#l:nat):
  vector a l -> vector b l -> cost (vector (a*b) l) M.(3*l+3)
let rec zip #_ #_ #_ v1 v2 =
  match v1 with | VNil -> 3 +~! VNil
                | VCons hd1 tl1 ->
  match v2 with | VCons hd2 tl2 -> 3 +! (VCons (hd1,hd2) <$> (zip tl1 tl2))

val sortedBy(#a:Type)(#l #n:nat): (a -> cost int n) -> vector a l -> GTot Type0
let rec sortedBy #_ #_ #_ f = function
  | VNil | VCons _ VNil -> true
  | VCons hd tl -> force (f hd) <= force (f (VCons?.hd tl)) /\ sortedBy f tl

val uniqueBy(#a:Type)(#l #n:nat): (a -> cost int n) -> vector a l -> GTot Type0
let rec uniqueBy #_ #_ #_ f = function
  | VNil | VCons _ VNil -> true
  | VCons hd tl -> force (f hd) < force (f (VCons?.hd tl))
                /\ sortedBy f tl /\ uniqueBy f tl

assume val mkUnique(#a:Type)(#l #n:nat):
  f:(a->cost int n) -> v:vector a l{sortedBy f v}
  -> cost (l':nat & v':vector a l'{uniqueBy f v'}) M.(l*n*12+6)


val of_t(#a:Type): a -> cost (vector a 1) 2
let of_t #_ x = incRet 2 (VCons x VNil)

val of_t2(#a:Type): a*a -> cost (vector a 2) 3
let of_t2 #_ (x,y) = incRet 3 (VCons x (VCons y VNil))

val of_t3(#a:Type): a*a*a -> cost (vector a 3) 4
let of_t3 #_ (x,y,z) = incRet 4 (VCons x (VCons y (VCons z VNil)))
(*)
val vcons_length_pos(#a:Type)(#l:nat): v:(vector a l){VCons? v}
  -> Lemma (l >= 1 /\ l-1 >=0)
let vcons_length_pos #_ #_ _ = ()

// length of a vector
val vlen(#a:Type)(#l:nat): vector a l -> nat
let vlen #_ #l _ = l

val for_all(#a:Type)(#l #n:nat):
  (a -> cost bool n) -> vector a l -> cost bool M.(l*(n+2)+2)
let rec for_all #_ #l #n f = function
  | VNil -> incRet 2 true
  | VCons hd tl -> 2 +! f hd >>= (fun x ->
      if x then for_all f tl else autoRet false)

val mem: #a:eqtype -> #l:nat -> a -> vector a l -> cost bool M.((l+1)*2)
let rec mem #a #l x = function
  | VNil -> incRet 2 false
  | VCons hd tl -> if hd = x then incRet M.((l+1)*2) true else 2 +! mem x tl

val count: #a:eqtype -> #l:nat -> a -> vector a l -> cost nat M.((l+1)*2)
let rec count #a #l x = function
  | VNil -> incRet 2 0
  | VCons hd tl -> if hd = x then (fun (x:nat) -> incRet 2 (x + 1) <: cost nat 2) =<< count x tl
                   else 2 +! count x tl

assume val foldl(#a #b:Type)(#n #l:nat):
  (a -> b -> cost a n) -> a -> vector b l -> cost a M.((n+2)*l+2)

(*)
private val is_sorted: #a:Type -> #l:nat -> #n:nat -> (a -> cost int n) -> vector a l
  -> cost bool M.((l*2)*(n+1)+2)
private let rec is_sorted #a #l #n f = function
  | VNil -> incRet 2 true
  | VCons _ VNil -> incRet M.(2*n+4) true
  | VCons x (VCons y tl) ->
    let head_sorted = (<=) <$> f x <*> f y in
    2 +! ((&&) <$> head_sorted <*> is_sorted f (VCons y tl))

val is_sorted_tail: #a:Type -> #l:nat -> #n:nat ->
  f:(a -> cost int n) -> v:vector a l{VCons? v}
  -> Lemma (forceT (is_sorted f v) ==> forceT (is_sorted f (VCons?.tl v)))
let rec is_sorted_tail #_ #_ #_ f = function
  | VNil | VCons _ VNil -> ()
  | VCons x (VCons y tl) ->
    let head_sorted = (<=) <$> f x <*> f y in
    force_liftM (&&) head_sorted;
    is_sorted_tail f (VCons y tl);
    force_ap ((&&) <$> head_sorted) (is_sorted f (VCons y tl));
    force_inc 2 ((&&) <$> head_sorted <*> is_sorted f (VCons y tl))

type sorted (#a:Type)(#l:nat)(#n:nat)
  (f:a -> cost int n)(v:vector a l) = force (is_sorted f v) = true
type sortedVector (#n:nat)
  (a:Type)(l:nat)(f:a ->cost int n) = v:vector a l {sorted f v}

val sorted_tail: #a:Type -> #l:nat -> #n:nat ->
  f:(a -> cost int n) -> v:vector a l{VCons? v}
  -> Lemma (sorted f v ==> sorted f (VCons?.tl v))
let sorted_tail #_ #_ #_ f v = is_sorted_tail f v

type permutation (#a:eqtype)(#l:nat)(v1:vector a l)(v2:vector a l) =
  forall (x:a). mem x v1 = mem x v2 /\ count x v1 = count x v2

assume val mergesort': #a:Type -> #l:nat -> #n:nat
  -> f:(a-> cost int n) -> vector a l
  -> cost (sortedVector a l f) M.(l*n*2 * Math.log_2 (l + 1))
assume val mergesort: #a:eqtype -> #l:nat -> #n:nat
  -> f:(a-> cost int n) -> v:vector a l
  -> cost (res:sortedVector a l f{permutation res v}) M.(l*n*2 * Math.log_2 (l + 1))

val is_unique: #a:eqtype -> #l:nat -> #n:nat -> #f:(a -> cost int n)
  -> sortedVector a l f -> cost bool M.((l+1)*2)
let rec is_unique #a #l #n #f v = match v with
  | VNil -> incRet 2 true
  | VCons _ VNil  -> incRet 4 true
  | VCons x (VCons y tl) ->
    sorted_tail f v;
    let tail_unique = is_unique #a #(l-1) #n #f (VCons y tl) in
    2 +! ((&&) (x<>y) <$> tail_unique)

type unique (#a:eqtype)(#l:nat)(#n:nat)(#f:a -> cost int n)
  (v:sortedVector a l f) = force (is_unique v) = true


val is_unique_tail: #a:eqtype -> #l:nat -> #n:nat -> #f:(a -> cost int n)
  -> v:sortedVector a l f{VCons? v}
  -> Lemma (forceT (is_unique v) ==> forceT (is_unique #a #(l-1) #n #f (sorted_tail f v; VCons?.tl v)))
let rec is_unique_tail #a #l #n #f v = match v with
  | VNil | VCons _ VNil -> ()
  | VCons x (VCons y tl) ->
    sorted_tail f v;
    let tail_unique = is_unique #a #(l-1) #n #f (VCons y tl) in
    force_liftM ((&&) (x <> y)) tail_unique;
    is_unique_tail #a #(l-1) #n #f (VCons y tl);
    force_inc 2 ((&&) (x<>y) <$> tail_unique)
(*)
val unique: #a:eqtype -> #l:nat -> #n:nat -> #f:(a -> cost int n)
  -> sortedVector a l f -> cost (l2:nat & sortedVector a l2 f) M.((l+1)*2)
let rec unique #a #l #n #f v = match v with
  | VNil -> incRet 2 (| 0, v |)
  | VCons _ VNil  -> incRet 4 (|1, v|)
  | VCons x (VCons y tl) ->
    sorted_tail f v;
    let tail = 2 +! unique #a #(l-1) #n #f (VCons y tl) in
    tail <&> (function
    | (|l2, tail|) -> if x <> y then (|l2+1, VCons x tail|)
    else (|l2, tail|))

(*)
// helps with vrev
private val vrev_help: #a:Type
  -> #l1:nat
  -> vector a l1
  -> #l2:nat
  -> vector a l2
  -> Tot (cost (vector a (l1+l2)) (M.(3 * l1) + 2)) (decreases l1)
private let rec vrev_help #a #l1 v1 #l2 v2 =
  match v1 with
  | VNil -> ret v2 `inc` 2
  | VCons hd tl ->
    (ret (VCons hd v2) `inc` 3)
    >>= (fun v2' -> vrev_help tl #(l2+1) v2')

// reverse a vector
val vrev: #a:Type -> #l:nat -> vector a l -> cost (vector a l) M.(3*l + 3)
let vrev #a #l v = vrev_help v VNil `inc` 1


val take: #a:Type -> #l:nat -> n:nat{n<=l} -> vector a l -> cost (vector a n) (n+1)
let rec take #a #l n v = match n with
  | 0 -> incRet 1 VNil
  | n -> match v with | VCons hd tl ->
      let n':nat = n-1 in
      1 +! (VCons hd <$> (take n' tl))

val split: #a:Type -> #l:nat -> i:pos{i<=l} -> vector a l
  -> cost (vector a i * vector a (l-i)) i
let rec split #a #l i (VCons hd tl) = match i with
  | 1 -> incRet 1 (VCons hd VNil, tl)
  | _ -> let i':pos = i-1 in
    1 +! ((fun (fst,snd : vector a i' * vector a (l-i)) -> VCons hd fst, snd) <$> split i' tl)


assume val chunkBy: #a:Type -> #n:nat -> chunkSize:pos -> vector a n
  -> cost (vector (vector a chunkSize) (n/chunkSize)) n
(*
let rec chunkBy #a #n size v =
  if n < size then incRet n VNil
  else
  split size v >>= (fun (fst, snd : vector a size * vector a (n-size)) ->
  FStar.Math.Lemmas.division_sub_lemma n size 1;
  let n':nat = n-size in
  if n' < size then incRet n' (VCons fst VNil) <: cost (vector (vector a size) (n/size)) (n-size)
  else admit()
    //VCons fst <$> (chunkBy size snd)
    )
*)

// maps f onto v, but also reverses it.
private val tail_map_help: #a:Type
  -> #b:Type
  -> #n: nat
  -> (a-> cost b n)
  -> #l1:nat
  -> vector a l1
  -> #l2:nat
  -> vector b l2
  -> Tot (cost (vector b (l1+l2)) ((n+3)*l1 + 2)) (decreases l1)
private let rec tail_map_help #a #b #n f #l1 v1 #l2 v2 =
  match v1 with
  | VNil -> ret v2 `inc` 2
  | VCons #_ #l_tl hd tl -> (f hd) >>=
      (fun f_hd -> ret (VCons f_hd v2) `inc` 3) >>=
      (fun v2'-> tail_map_help f #l_tl tl v2')

// maps f onto v
val vmap: #a:Type
  -> #b:Type
  -> #n: nat
  -> (f:a->cost b n)
  -> #l:nat
  -> vector a l
  -> cost (vector b l) ((n+6) * l + 5)
let vmap #a #b #n f #l v =
  (tail_map_help f v VNil)
  >>= vrev

val vfoldl: #a:Type
  -> #b:Type
  -> #n:nat
  -> (a -> b -> cost a n)
  -> a
  -> #l:nat
  -> vector b l
  -> Tot (cost a ((n+2)*l + 2)) (decreases l)
let rec vfoldl #a #b #n f acc #l v =
  match v with
  | VNil -> ret acc `inc` 2
  | VCons hd tl ->
    (f acc hd `inc` 2)
    >>= (fun acc' -> vfoldl f acc' tl)
