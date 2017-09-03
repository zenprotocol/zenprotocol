#light "off"
module FStar.UInt
open Prims
open FStar.Pervasives

let pow2_values : Prims.nat  ->  Prims.unit = (fun ( x  :  Prims.nat ) -> ())


let max_int : Prims.nat  ->  Prims.int = (fun ( n  :  Prims.nat ) -> ((Prims.pow2 n) - (Prims.parse_int "1")))


let min_int : Prims.nat  ->  Prims.int = (fun ( n  :  Prims.nat ) -> (Prims.parse_int "0"))


let fits : Prims.int  ->  Prims.nat  ->  Prims.bool = (fun ( x  :  Prims.int ) ( n  :  Prims.nat ) -> (((min_int n) <= x) && (x <= (max_int n))))


type ('Ax, 'An) size = | S of Prims.unit


type 'An uint_t = | UInt of Prims.int


let zero : Prims.nat  ->  Prims.unit uint_t = (fun ( n  :  Prims.nat ) -> UInt (Prims.parse_int "0"))


let one : Prims.pos  ->  Prims.unit uint_t = (fun ( n  :  Prims.pos ) -> UInt (Prims.parse_int "1"))


let ones : Prims.nat  ->  Prims.unit uint_t = (fun ( n  :  Prims.nat ) -> UInt (max_int n))


let incr : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t = (fun ( uu___13_278  :  Prims.nat ) (UInt x  :  Prims.unit uint_t ) -> UInt (x + (Prims.parse_int "1")))


let decr : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t = (fun ( uu___14_358  :  Prims.nat ) (UInt a : Prims.unit uint_t ) -> UInt (a - (Prims.parse_int "1")))


let add : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.unit uint_t = (fun ( uu___15_452  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> UInt (a + b))


let add_mod : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.unit uint_t = (fun ( n  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> UInt ((a + b) mod (Prims.pow2 n)))


let sub : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.unit uint_t = (fun ( uu___16_693  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> UInt (a - b))


let sub_mod : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.unit uint_t = (fun ( n  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> UInt ((a - b) mod (Prims.pow2 n)))


let mul : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.unit uint_t = (fun ( uu___17_934  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> UInt (a * b))


let mul_mod : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.unit uint_t = (fun ( n  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> UInt ((a * b) mod (Prims.pow2 n)))


let div : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.unit uint_t = (fun ( uu___18_1178  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> UInt (a / b))


let mod_ : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.unit uint_t = (fun ( uu___19_1299  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> UInt (a - ((a / b) * b)))


let eq : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.bool = (fun ( uu___20_1430  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> (a = b))


let gt : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.bool = (fun ( uu___21_1524  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> (a > b))


let gte : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.bool = (fun ( uu___22_1623  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> (a >= b))


let lt : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.bool = (fun ( uu___23_1722  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> (a < b))


let lte : Prims.nat  ->  Prims.unit uint_t  ->  Prims.unit uint_t  ->  Prims.bool = (fun ( uu___24_1821  :  Prims.nat ) (UInt a : Prims.unit uint_t ) (UInt b : Prims.unit uint_t ) -> (a <= b))


let to_uint_t : Prims.nat  ->  Prims.int  ->  Prims.unit uint_t = (fun ( m  :  Prims.nat ) ( a  :  Prims.int ) -> UInt (a mod (Prims.pow2 m)))
