module FStar.Int

val pow2_values: x:nat -> Lemma
  (requires True)
  (ensures (let p = pow2 x in
   match x with
   | 0  -> p=1
   | 1  -> p=2
   | 8  -> p=256
   | 16 -> p=65536
   | 31 -> p=2147483648
   | 32 -> p=4294967296
   | 63 -> p=9223372036854775808
   | 64 -> p=18446744073709551616
   | _  -> True))
  [SMTPat (pow2 x)]
let pow2_values x =
   match x with
   | 0  -> assert_norm (pow2 0 == 1)
   | 1  -> assert_norm (pow2 1 == 2)
   | 8  -> assert_norm (pow2 8 == 256)
   | 16 -> assert_norm (pow2 16 == 65536)
   | 31 -> assert_norm (pow2 31 == 2147483648)
   | 32 -> assert_norm (pow2 32 == 4294967296)
   | 63 -> assert_norm (pow2 63 == 9223372036854775808)
   | 64 -> assert_norm (pow2 64 == 18446744073709551616)
   | _  -> ()

(* NOTE: anything that you fix/update here should be reflected in [FStar.UInt.fst], which is mostly
 * a copy-paste of this module. *)

(* Necessary mathematical functions. Note: should these go into [prims.fst] or something else? *)

// 'flooring' division
let op_Slash (a:int) (b:int{b <> 0}) : int =
  if (a >= 0 && b < 0) || (a < 0 && b >= 0) then -(abs a / abs b)
  else abs a / abs b

// Euclidian division
let div_eucl (a:int) (b:nonzero) : int =
  if a < 0 then
    if a % b = 0 then -(-a/b) else -(-a/b) -1
  else
    a / b
let op_Slash_Percent = div_eucl

// 'Circular modulo operator : wraps into [-p/2; p/2[
let op_At_Percent (v:int) (p:int{p>0/\ p%2=0}) : int =
  let m = v % p in if m >= p/2 then m - p else m

(* Specs *)
let max_int (n:pos) : int = pow2 (n-1) - 1
let min_int (n:pos) : int = - (pow2 (n-1))

let fits (x:int) (n:pos) : bool = min_int n <= x && x <= max_int n
let size (x:int) (n:pos) : Type0 = b2t(fits x n)

(* Machine integer type *)
type int_t (n:pos) = x:int{size x n}

(* Addition primitives *)
val add(#n:pos): a:int_t n -> b:int_t n -> Pure (int_t n)
  (requires (size (a + b) n))
  (ensures (fun _ -> True))
let add(#_) a b = a + b

val checked_add(#n:pos): int_t n -> int_t n -> option (int_t n)
let checked_add #n a b = if fits (a + b) n then Some (a + b) else None

val add_mod(#n:pos): int_t n -> int_t n -> int_t n
let add_mod #n a b = (a + b) @% (pow2 n)

(* Subtraction primitives *)
val sub(#n:pos): a:int_t n -> b:int_t n -> Pure (int_t n)
  (requires (size (a - b) n))
  (ensures (fun _ -> True))
let sub(#_) a b = a - b

val sub_mod(#n:pos): a:int_t n -> b:int_t n -> int_t n
let sub_mod #n a b = (a - b) @% (pow2 n)

val checked_sub(#n:pos): int_t n -> int_t n -> option (int_t n)
let checked_sub #n a b = if fits (a - b) n then Some (a - b) else None

(* Multiplication primitives *)
val mul(#n:pos): a:int_t n -> b:int_t n -> Pure (int_t n)
  (requires (size (a * b) n))
  (ensures (fun _ -> True))
let mul(#_) a b = a * b

val mul_mod(#n:pos): a:int_t n -> b:int_t n -> int_t n
let mul_mod #n a b = (a * b) @% (pow2 n)

val checked_mul(#n:pos): int_t n -> int_t n -> option (int_t n)
let checked_mul #n a b = if fits (a * b) n then Some (a * b) else None

(* Division primitives *)
val div(#n:pos): a:int_t n -> b:int_t n{b <> 0} -> Pure (int_t n)
  (requires (size (a / b) n))
  (ensures (fun c -> b <> 0 ==> a / b = c))
let div(#_) a b = a / b

val checked_div(#n:pos): a:int_t n -> b:int_t n -> option (int_t n)
let checked_div(#n) a b =
  if b = 0 then None else
  if fits (a / b) n then Some (a / b) else None

(* Modulo primitives *)
#set-options "--z3rlimit 13616400"
val mod(#n:pos): a:int_t n -> b:int_t n{b <> 0} -> int_t n
let mod(#_) a b = a - ((a/b) * b)
#reset-options
(*
(* Bitwise operators *)
assume val logand: #n:pos -> int_t n -> int_t n -> Tot (int_t n)
assume val logxor: #n:pos -> int_t n -> int_t n -> Tot (int_t n)
assume val logor: #n:pos -> int_t n -> int_t n -> Tot (int_t n)
assume val lognot: #n:pos -> int_t n -> Tot (int_t n)
*)
(* Comparison operators *)
val eq(#n:pos): int_t n -> int_t n -> bool
let eq #_ a b = (a = b)

val gt(#n:pos): int_t n -> int_t n -> bool
let gt #_ a b = (a > b)

val gte(#n:pos): int_t n -> int_t n -> bool
let gte #_ a b  = (a >= b)

val lt(#n:pos): int_t n -> int_t n -> bool
let lt #_ a b = (a < b)

val lte(#n:pos): int_t n -> int_t n -> bool
let lte #_ a b = (a <= b)

(* Casts *)
val to_int_t: m:pos -> int -> int_t m
let to_int_t m a = a @% pow2 m
(*
(* Shift operators *)
assume val shift_right: #n:pos -> a:int_t n -> s:nat -> Pure (int_t n)
  (requires True)
  (ensures (fun b -> b = (a /% (pow2 s))))

val shift_left: #n:pos -> a:int_t n -> s:nat -> Tot (int_t n)
let shift_left #n a s = (a * (pow2 s)) @% (pow2 n)
*)
