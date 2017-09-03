#light "off"
module Zen.ArrayRealized
open Prims
open FStar.Pervasives

type ('Aa, 'Auu____9) array = A of 'Aa Microsoft.FSharp.Core.array


let empty (_:Prims.unit) : array<'Aa,Prims.unit> = A [||]
let at (_:Prims.nat) (A a: ('Aa,Prims.unit) array) (n:Prims.nat) : ('Aa , Prims.unit) Zen.Cost.cost =
  Zen.Cost.ret a.[Microsoft.FSharp.Core.Operators.int n]


let get = at

let ofVec = (fun ( uu___376_239  :  Prims.nat ) ( v  :  ('Auu___375_204, Prims.unit) Zen.Vector.t ) -> (Zen.Cost.autoRet ((Prims.parse_int "2") * (uu___376_239 + (Prims.parse_int "1"))) v))


let init = (fun ( a132  :  Prims.nat ) ( a133  :  Prims.nat ) ( a134  :  Prims.nat  ->  ('Aa, Prims.unit) Zen.Cost.cost ) -> ((Prims.unsafe_coerce (fun ( n  :  Prims.nat ) ( l  :  Prims.nat ) ( f  :  Prims.nat  ->  ('Aa, Prims.unit) Zen.Cost.cost ) -> (failwith "Not yet implemented:init"))) a132 a133 a134))


let append = (fun ( a135  :  Prims.nat ) ( a136  :  'Aa ) ( a137  :  ('Aa, Prims.unit) array ) -> ((Prims.unsafe_coerce (fun ( l  :  Prims.nat ) ( uu____431  :  'Aa ) ( uu____432  :  ('Aa, Prims.unit) array ) -> (failwith "Not yet implemented:append"))) a135 a136 a137))
