module FStar.UInt64

module Checked = FSharp.Core.Operators.Checked
open FStar.Pervasives.Native

type uint64 = System.UInt64
type uint8 = int
type t = uint64
type t' = t

let n = Prims.parse_int "64"
let v (x:uint64) : FStar.UInt.uint_t<'a> = FStar.UInt.UInt (Prims.parse_int (x.ToString()))

let zero = 0UL
let one = 1UL
let ones = uint64.MaxValue

let add (a:uint64) (b:uint64) : uint64 = a + b
let add_mod a b = add a b
let checked_add a b : option<uint64> = try Some (Checked.(+) a b) with | _ -> None

let sub (a:uint64) (b:uint64) : uint64 = a - b
let sub_mod a b = sub a b
let checked_sub a b : option<uint64> = try Some (Checked.(-) a b) with | _ -> None

let mul (a:uint64) (b:uint64) : uint64 = a * b
let mul_mod a b = mul a b
let checked_mul a b : option<uint64> = try Some (Checked.(*) a b) with | _ -> None

let div (a:uint64) (b:uint64) : uint64 = a / b
let checked_div a b : option<uint64> = try Some (a / b) with | _ -> None

let rem (a:uint64) (b:uint64) : uint64 = a % b

let uint_to_t s : uint64 = uint64.Parse(s.ToString())

(* Comparison operators *)

let eq (a:uint64) (b:uint64) : bool = a = b
let gt (a:uint64) (b:uint64) : bool = a > b
let gte (a:uint64) (b:uint64) : bool = a >= b
let lt (a:uint64) (b:uint64) : bool = a < b
let lte (a:uint64) (b:uint64) : bool =  a <= b

(* Constant time operator (TODO!) *)

(* Infix notations *)

let op_Plus_Hat = add
let op_Plus_Percent_Hat = add_mod
let op_Plus_Question_Hat = checked_add
let op_Subtraction_Hat = sub
let op_Subtraction_Percent_Hat = sub_mod
let op_Subtraction_Question_Hat = checked_sub
let op_Star_Hat = mul
let op_Star_Percent_Hat = mul_mod
let op_Star_Question_Hat = checked_mul
let op_Slash_Hat = div
let op_Slash_Question_Hat = checked_div
let op_Percent_Hat = rem
let op_Equals_Hat = eq
let op_Greater_Hat = gt
let op_Greater_Equals_Hat = gte
let op_Less_Hat = lt
let op_Less_Equals_Hat = lte

let to_string s = s.ToString()
let of_string s : uint64 = uint64.Parse s
