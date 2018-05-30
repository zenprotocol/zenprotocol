#light "off"
module FStar.Int
open Prims
open FStar.Pervasives

let pow2_values : Prims.nat  ->  Prims.unit = (fun ( x  :  Prims.nat ) -> ())


let op_Slash : Prims.int  ->  Prims.int  ->  Prims.int = (fun ( a  :  Prims.int ) ( b  :  Prims.int ) -> (match ((((a >= (Prims.parse_int "0")) && (b < (Prims.parse_int "0"))) || ((a < (Prims.parse_int "0")) && (b >= (Prims.parse_int "0"))))) with
| true -> begin
(- (((Prims.abs a) / (Prims.abs b))))
end
| uu____80 -> begin
((Prims.abs a) / (Prims.abs b))
end))


let div_eucl : Prims.int  ->  Prims.nonzero  ->  Prims.int = (fun ( a  :  Prims.int ) ( b  :  Prims.nonzero ) -> (match ((a < (Prims.parse_int "0"))) with
| true -> begin
(match (((a mod b) = (Prims.parse_int "0"))) with
| true -> begin
(- ((- ((op_Slash a b)))))
end
| uu____105 -> begin
((- ((- ((op_Slash a b))))) - (Prims.parse_int "1"))
end)
end
| uu____110 -> begin
(op_Slash a b)
end))


let op_Slash_Percent : Prims.int  ->  Prims.nonzero  ->  Prims.int = div_eucl


let op_At_Percent : Prims.int  ->  Prims.int  ->  Prims.int = (fun ( v  :  Prims.int ) ( p  :  Prims.int ) -> (match (((v mod p) >= (op_Slash p (Prims.parse_int "2")))) with
| true -> begin
((v mod p) - p)
end
| uu____138 -> begin
(v mod p)
end))


let max_int : Prims.pos  ->  Prims.int = (fun ( n  :  Prims.pos ) -> ((Prims.pow2 (n - (Prims.parse_int "1"))) - (Prims.parse_int "1")))


let min_int : Prims.pos  ->  Prims.int = (fun ( n  :  Prims.pos ) -> (- ((Prims.pow2 (n - (Prims.parse_int "1"))))))


let fits : Prims.int  ->  Prims.pos  ->  Prims.bool = (fun ( x  :  Prims.int ) ( n  :  Prims.pos ) -> (((min_int n) <= x) && (x <= (max_int n))))


type ('Ax, 'An) size = Prims.unit

type 'An int_t = Prims.int


let add : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.unit int_t = (fun ( uu___11_281  :  Prims.pos ) (  a  :  Prims.unit int_t ) (  b  :  Prims.unit int_t ) ->  (a + b))


let add_mod : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.unit int_t = (fun ( n  :  Prims.pos ) (  a  :  Prims.unit int_t ) (  b  :  Prims.unit int_t ) ->  (op_At_Percent (a + b) (Prims.pow2 n)))


let sub : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.unit int_t = (fun ( uu___12_544  :  Prims.pos ) ( a  :  Prims.unit int_t ) (  b  :  Prims.unit int_t ) ->  (a - b))


let sub_mod : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.unit int_t = (fun ( n  :  Prims.pos ) ( a  :  Prims.unit int_t ) ( b  :  Prims.unit int_t ) -> (op_At_Percent (a - b) (Prims.pow2 n)))


let mul : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.unit int_t = (fun ( uu___13_807  :  Prims.pos ) ( a  :  Prims.unit int_t ) ( b  :  Prims.unit int_t ) ->  (a * b))


let mul_mod : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.unit int_t = (fun ( n  :  Prims.pos ) ( a  :  Prims.unit int_t ) (  b  :  Prims.unit int_t ) ->  (op_At_Percent (a * b) (Prims.pow2 n)))


let div : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.unit int_t = (fun ( uu___14_1073  :  Prims.pos ) ( a  :  Prims.unit int_t ) ( b  :  Prims.unit int_t ) ->  (op_Slash a b))


let mod_ : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.unit int_t = (fun ( uu___15_1205  :  Prims.pos ) ( a  :  Prims.unit int_t ) ( b  :  Prims.unit int_t ) ->  (a - ((op_Slash a b) * b)))


let eq : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.bool = (fun ( uu___16_1350  :  Prims.pos ) ( a  :  Prims.unit int_t ) ( b  :  Prims.unit int_t ) -> (a = b))


let gt : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.bool = (fun ( uu___17_1452  :  Prims.pos ) ( a  :  Prims.unit int_t ) ( b  :  Prims.unit int_t ) -> (a > b))


let gte : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.bool = (fun ( uu___18_1560  :  Prims.pos ) ( a  :  Prims.unit int_t ) ( b  :  Prims.unit int_t ) -> (a >= b))


let lt : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.bool = (fun ( uu___19_1668  :  Prims.pos ) ( a  :  Prims.unit int_t ) ( b  :  Prims.unit int_t ) -> (a < b))


let lte : Prims.pos  ->  Prims.unit int_t  ->  Prims.unit int_t  ->  Prims.bool = (fun ( uu___20_1776  :  Prims.pos ) ( a  :  Prims.unit int_t ) ( b  :  Prims.unit int_t ) -> (a <= b))


let to_int_t : Prims.pos  ->  Prims.int  ->  Prims.unit int_t = (fun ( m  :  Prims.pos ) (a  :  Prims.int ) ->  (op_At_Percent a (Prims.pow2 m)))
