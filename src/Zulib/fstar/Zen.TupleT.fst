(** This module defines monad transformers for tuples and cost. *)
module Zen.TupleT

open Zen.Base
open Zen.Cost
module M = FStar.Mul
module T = Zen.Tuple

val join(#a #b:Type)(#m #n:nat): cost a n ** cost b m -> cost (a**b) (n+m)
let join #_ #_ #_ #_ (mx,my) = Mktuple2 <$> mx <*> my

val join3(#a #b #c:Type)(#n1 #n2 #n3:nat): cost a n1 ** cost b n2 ** cost c n3
  -> cost (a**b**c) (n1+n2+n3)
let join3 #_ #_ #_ #_ #_ #_ (mx,my,mz) =
  Mktuple3 <$> mx <*> my <*> mz

val join4(#a #b #c #d:Type)(#n1 #n2 #n3 #n4:nat):
  cost a n1 ** cost b n2 ** cost c n3 ** cost d n4 -> cost (a**b**c**d) (n1+n2+n3+n4)
let join4 #_ #_ #_ #_ #_ #_ #_ #_ (mw,mx,my,mz) =
  Mktuple4 <$> mw <*> mx <*> my <*> mz

val mapJoin(#a #b:Type)(#n:nat): (a -> cost b n) -> a**a -> cost (b**b) M.(n*2)
let mapJoin #_ #_ #_ mf = T.map mf >> join

val mapJoin3(#a #b:Type)(#n:nat): (a -> cost b n) -> a**a**a -> cost (b**b**b) M.(n*3)
let mapJoin3 #_ #_ #_ mf = T.map3 mf >> join3

val mapJoin4(#a #b:Type)(#n:nat): (a -> cost b n) -> a**a**a**a -> cost (b**b**b**b) M.(n*4)
let mapJoin4 #_ #_ #_ mf = T.map4 mf >> join4

val bind(#a #b:Type)(#n1 #n2 #n3:nat): cost a n1 ** cost a n2 -> (a -> cost b n3)
  -> cost b (n1+n3) ** cost b (n2+n3)
let bind #_ #_ #_ #_ #_ (mx,my) mf =
  mx >>= mf, my >>= mf

val bind3(#a #b:Type)(#n1 #n2 #n3 #n4:nat):
  cost a n1 ** cost a n2 ** cost a n3 -> (a -> cost b n4)
  -> cost b (n1+n4) ** cost b (n2+n4) ** cost b (n3+n4)
let bind3 #_ #_ #_ #_ #_ #_ (mx,my,mz) mf =
  mx >>= mf, my >>= mf, mz >>= mf

val bind4(#a #b:Type)(#n1 #n2 #n3 #n4 #n5:nat):
  cost a n1 ** cost a n2 ** cost a n3 ** cost a n4 -> (a -> cost b n5)
 -> cost b (n1+n5) ** cost b (n2+n5) ** cost b (n3+n5) ** cost b (n4+n5)
let bind4 #a #b #_ #_ #_ #_ #_ (mw,mx,my,mz) mf =
  mw >>= mf, mx >>= mf, my >>= mf, mz >>= mf

val bindJoin(#a #b:Type)(#n1 #n2 #n3:nat):
  cost a n1 ** cost a n2 -> (a -> cost b n3) -> cost (b**b) M.(n1+n2+(2*n3))
let bindJoin #_ #_ #_ #_ #_ mt mf = join (bind mt mf)

val bindJoin3(#a #b:Type)(#n1 #n2 #n3 #n4:nat):
  cost a n1 ** cost a n2 ** cost a n3 -> (a -> cost b n4)
  -> cost (b**b**b) M.(n1+n2+n3+(3*n4))
let bindJoin3 #_ #_ #_ #_ #_ #_ mt mf = join3 (bind3 mt mf)

val bindJoin4(#a #b:Type)(#n1 #n2 #n3 #n4 #n5:nat):
  cost a n1 ** cost a n2 ** cost a n3 ** cost a n4 -> (a -> cost b n5)
  -> cost (b**b**b**b) M.(n1+n2+n3+n4+(4*n5))
let bindJoin4 #_ #_ #_ #_ #_ #_ #_ mt mf = join4 (bind4 mt mf)
