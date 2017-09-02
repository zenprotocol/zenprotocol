module FStar.Int64

type int64 = System.Int64
type uint8 = int
type t = int64
type t' = t

let n = Prims.parse_int "64"
let v (x:int64) : FStar.UInt.uint_t<'a> = FStar.UInt.UInt (Prims.parse_int (x.ToString()))

let zero = 0L
let one = 1L
let ones = int64.MaxValue

let add (a:int64) (b:int64) : int64 = a + b
let add_mod a b = add a b

let sub (a:int64) (b:int64) : int64 = a - b
let sub_mod a b = sub a b

let mul (a:int64) (b:int64) : int64 = a * b
let mul_mod a b = mul a b

let div (a:int64) (b:int64) : int64 = a / b

let rem (a:int64) (b:int64) : int64 = a % b

let uint_to_t s : int64 = int64.Parse(s.ToString())

(* Comparison operators *)

let eq (a:int64) (b:int64) : bool = a = b
let gt (a:int64) (b:int64) : bool = a > b
let gte (a:int64) (b:int64) : bool = a >= b
let lt (a:int64) (b:int64) : bool = a < b
let lte (a:int64) (b:int64) : bool =  a <= b

(* Infix notations *)

let op_Plus_Hat = add
let op_Plus_Percent_Hat = add_mod
let op_Subtraction_Hat = sub
let op_Subtraction_Percent_Hat = sub_mod
let op_Star_Hat = mul
let op_Star_Percent_Hat = mul_mod
let op_Slash_Hat = div
let op_Percent_Hat = rem
let op_Equals_Hat = eq
let op_Greater_Hat = gt
let op_Greater_Equals_Hat = gte
let op_Less_Hat = lt
let op_Less_Equals_Hat = lte

let to_string s = s.ToString()
let of_string s : int64 = int64.Parse s
