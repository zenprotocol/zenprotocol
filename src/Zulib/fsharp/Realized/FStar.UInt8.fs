module FStar.UInt8
//open Prims

module Checked = FSharp.Core.Operators.Checked
open FStar.Pervasives.Native

type uint8 = System.Byte
type byte = uint8
type t = uint8
type t' = t

let n = Prims.parse_int "8"
let v (x:uint8) : FStar.UInt.uint_t<'a> = FStar.UInt.UInt (Prims.parse_int (x.ToString()))

let zero = 0uy
let one = 1uy
let ones = 255uy

let add (a:uint8) (b:uint8) : uint8 = a + b
let add_mod a b = (add a b) &&& 255uy
let checked_add a b : option<uint32> = try Some (Checked.(+) a b) with | _ -> None

let sub (a:uint8) (b:uint8) : uint8 = a - b
let sub_mod a b = (sub a b) &&& 255uy
let checked_sub a b : option<uint8> = try Some (Checked.(-) a b) with | _ -> None

let mul (a:uint8) (b:uint8) : uint8 = a * b
let mul_mod a b = (mul a b) &&& 255uy
let checked_mul a b : option<uint8> = try Some (Checked.(*) a b) with | _ -> None

let div (a:uint8) (b:uint8) : uint8 = a / b
let checked_div a b : option<uint8> = try Some (a / b) with | _ -> None

let rem (a:uint8) (b:uint8) : uint8 = a % b

let uint_to_t s : uint8 = uint8.Parse(s.ToString())

(* Comparison operators *)

let eq (a:uint8) (b:uint8) : bool = a = b
let gt (a:uint8) (b:uint8) : bool =  a > b
let gte (a:uint8) (b:uint8) : bool = a >= b
let lt (a:uint8) (b:uint8) : bool = a < b
let lte (a:uint8) (b:uint8) : bool =  a <= b

(* Constant time comparison operators, TODO: check and implement efficiently *)

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

let of_string s = int s
let to_string s = s.ToString()
//let to_string_hex s = Printf.sprintf "%02x" s
//let to_int s = s
