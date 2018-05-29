module FStar.Int64

unfold let n = 64

open FStar.Int
open Zen.Option

abstract type t =
    | Mk: v:int_t n -> t

abstract let v (x:t) : int_t n = x.v

abstract val int_to_t: x:int_t n -> Pure t
  (requires True)
  (ensures (fun y -> v y = x))
abstract let int_to_t x = Mk x

let uv_inv (x : t) : Lemma
  (ensures (int_to_t (v x) == x))
  [SMTPat (v x)] = ()

let vu_inv (x : int_t n) : Lemma
  (ensures (v (int_to_t x) == x))
  [SMTPat (int_to_t x)] = ()

let v_inj (x1 x2: t): Lemma
  (requires (v x1 == v x2))
  (ensures (x1 == x2))
  = ()

abstract val add: a:t -> b:t -> Pure t
  (requires (size (v a + v b) n))
  (ensures (fun c -> v a + v b = v c))
abstract let add a b = Mk (add (v a) (v b))

abstract val add_mod: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a + v b) @% pow2 n = v c))
abstract let add_mod a b = Mk (add_mod (v a) (v b))

abstract val checked_add: a:t -> b:t -> option t
abstract let checked_add a b = map Mk (checked_add (v a) (v b))

(* Subtraction primitives *)
abstract val sub: a:t -> b:t -> Pure t
  (requires (size (v a - v b) n))
  (ensures (fun c -> v a - v b = v c))
abstract let sub a b = Mk (sub (v a) (v b))

abstract val sub_mod: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a - v b) @% pow2 n = v c))
abstract let sub_mod a b = Mk (sub_mod (v a) (v b))

abstract val checked_sub: a:t -> b:t -> option t
abstract let checked_sub a b = map Mk (checked_sub (v a) (v b))

(* Multiplication primitives *)
abstract val mul: a:t -> b:t -> Pure t
  (requires (size (v a * v b) n))
  (ensures (fun c -> v a * v b = v c))
abstract let mul a b = Mk (mul (v a) (v b))

abstract val mul_mod: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a * v b) @% pow2 n = v c))
abstract let mul_mod a b = Mk (mul_mod (v a) (v b))

abstract val checked_mul: a:t -> b:t -> option t
abstract let checked_mul a b = map Mk (checked_mul (v a) (v b))

(*
val mul_div: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a * v b) / pow2 n = v c))
let mul_div a b = Mk (mul_div (v a) (v b))
*)

(* Division primitives *)
abstract val div: a:t -> b:t{v b <> 0} -> Pure t
  (requires (size (v a / v b) n))
  (ensures (fun c -> v a / v b = v c))
abstract let div a b = Mk (div (v a) (v b))

abstract val checked_div: a:t -> b:t -> option t
abstract let checked_div a b = map Mk (checked_div (v a) (v b))

(* Modulo primitives *)
abstract val rem: a:t -> b:t{v b <> 0} -> Pure t
  (requires True)
  (ensures (fun c ->
    v a - ((v a / v b) * v b) = v c))
abstract let rem a b = Mk (mod (v a) (v b))

(*
(* Bitwise operators *)
val logand: t -> t -> Tot t
let logand a b = Mk (logand (v a) (v b))
val logxor: t -> t -> Tot t
let logxor a b = Mk (logxor (v a) (v b))
val logor: t -> t -> Tot t
let logor a b = Mk (logor (v a) (v b))
val lognot: t -> Tot t
let lognot a = Mk (lognot (v a))
*)

(* Comparison operators *)

let eq  (a:t) (b:t) : bool = eq  #n (v a) (v b)
let gt  (a:t) (b:t) : bool = gt  #n (v a) (v b)
let gte (a:t) (b:t) : bool = gte #n (v a) (v b)
let lt  (a:t) (b:t) : bool = lt  #n (v a) (v b)
let lte (a:t) (b:t) : bool = lte #n (v a) (v b)
(*
assume val eq_mask: a:t -> b:t -> Tot (c:t{(v a = v b ==> v c = pow2 n - 1) /\ (v a <> v b ==> v c = 0)})
assume val gte_mask: a:t -> b:t -> Tot (c:t{(v a >= v b ==> v c = pow2 n - 1) /\ (v a < v b ==> v c = 0)})
*)
(* Infix notations *)
unfold let op_Plus_Hat = add
unfold let op_Plus_Percent_Hat = add_mod
unfold let (+?^) = checked_add
unfold let op_Subtraction_Hat = sub
unfold let op_Subtraction_Percent_Hat = sub_mod
unfold let (-?^) = checked_sub
unfold let op_Star_Hat = mul
unfold let op_Star_Percent_Hat = mul_mod
unfold let ( *?^) = checked_mul
//unfold let op_Star_Slash_Hat = mul_div
unfold let op_Slash_Hat = div
unfold let (/?^) = checked_div
unfold let op_Percent_Hat = rem
//unfold let op_Hat_Hat = logxor
//unfold let op_Amp_Hat = logand
//unfold let op_Bar_Hat = logor
//unfold let op_Less_Less_Hat = shift_left
//unfold let op_Greater_Greater_Hat = shift_right
unfold let op_Equals_Hat = eq
unfold let op_Greater_Hat = gt
unfold let op_Greater_Equals_Hat = gte
unfold let op_Less_Hat = lt
unfold let op_Less_Equals_Hat = lte

(* To input / output constants *)
assume val to_string: t -> string
assume val of_string: string -> t
