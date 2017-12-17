module FStar.Pervasives

(* This is a file from the core library, dependencies must be explicit *)
open Prims
include FStar.Pervasives.Native

noeq type result (a:Type) =
  | OK   : v:a -> result a
  | EX   : e:exn -> result a
  | ERR : msg:string -> result a

(* An SMT-pattern to control unfolding inductives;
   In a proof, you can say `allow_inversion (option a)`
   to allow the SMT solver. cf. allow_inversion below
 *)
let inversion (a:Type) = True

let allow_inversion (a:Type)
  : Pure unit (requires True) (ensures (fun x -> inversion a))
  = ()

//allowing inverting option without having to globally increase the fuel just for this
val invertOption : a:Type -> Lemma
  (requires True)
  (ensures (forall (x:option a). None? x \/ Some? x))
  [SMTPat (option a)]
let invertOption a = allow_inversion (option a)

type either 'a 'b =
  | Inl : v:'a -> either 'a 'b
  | Inr : v:'b -> either 'a 'b

val dfst : #a:Type -> #b:(a -> GTot Type) -> dtuple2 a b -> Tot a
let dfst #a #b t = Mkdtuple2?._1 t

val dsnd : #a:Type -> #b:(a -> GTot Type) -> t:dtuple2 a b -> Tot (b (Mkdtuple2?._1 t))
let dsnd #a #b t = Mkdtuple2?._2 t

(* Concrete syntax (x:a & y:b x & c x y) *)
unopteq type dtuple3 (a:Type)
             (b:(a -> GTot Type))
             (c:(x:a -> b x -> GTot Type)) =
  | Mkdtuple3:_1:a
             -> _2:b _1
             -> _3:c _1 _2
             -> dtuple3 a b c

(* Concrete syntax (x:a & y:b x & z:c x y & d x y z) *)
unopteq type dtuple4 (a:Type)
             (b:(x:a -> GTot Type))
             (c:(x:a -> b x -> GTot Type))
             (d:(x:a -> y:b x -> z:c x y -> GTot Type)) =
 | Mkdtuple4:_1:a
           -> _2:b _1
           -> _3:c _1 _2
           -> _4:d _1 _2 _3
           -> dtuple4 a b c d

val ignore: #a:Type -> a -> Tot unit
let ignore #a x = ()
irreducible
let rec false_elim (#a:Type) (u:unit{false}) : Tot a = false_elim ()

(* For the compiler. Use as follows:
 *
 * [@ PpxDerivingShow ]
 * type t = A | B
 *
 * The resulting OCaml extracted type definition will have [@@ ppx_deriving show] attached to it. *)
type __internal_ocaml_attributes =
  | PpxDerivingShow
  | PpxDerivingShowConstant of string
  | CInline
  | Substitute
  | Gc
  | Comment of string
