(** This module defines monad transformers for option and cost. *)
module Zen.OptionT

open Zen.Cost
open Zen.Base
module Opt = Zen.Option

val none(#a:Type): cost (option a) 0
let none #_ = ret None

val some(#a:Type): a -> cost (option a) 0
let some(#_) = ret << Some

val incNone(#a:Type): n:nat -> cost (option a) n
let incNone #_ n = inc none n

val incSome(#a:Type): n:nat -> a -> cost (option a) n
let incSome(#_) n x = inc (some x) n

val lift(#a:Type)(#n:nat): cost a n -> cost (option a) n
let lift #_ #_ = liftM Some

val incLift(#a:Type)(#m:nat): n:nat -> cost a m -> cost (option a) (m+n)
let incLift(#_)(#_) n mx = inc (lift mx) n

val bind(#a #b:Type)(#m #n:nat): cost (option a) m -> (a -> cost (option b) n)
  -> cost (option b) (m+n)
let bind #_ #_ #_ #n mx f = mx >>= (function | None -> incNone n
                                             | Some x -> f x)

val bind2(#a #b #c:Type)(#n1 #n2 #n3:nat):
 cost (option a) n1 -> cost (option b) n2 -> (a -> b -> cost (option c) n3)
 -> cost (option c) (n1+n2+n3)
let bind2 #_ #_ #_ #_ #_ #_ mx my f = mx `bind` (fun x ->
                                      my `bind` (fun y ->
                                      f x y))

val bind3(#a #b #c #d:Type)(#n1 #n2 #n3 #n4:nat):
 cost (option a) n1 -> cost (option b) n2 -> cost (option c) n3
 -> (a -> b -> c -> cost (option d) n4)
 -> cost (option d) (n1+n2+n3+n4)
let bind3 #_ #_ #_ #_ #_ #_ #_ #_ mx my mz f = mx `bind` (fun x ->
                                               my `bind` (fun y ->
                                               mz `bind` (fun z ->
                                               f x y z)))

val map(#a #b:Type)(#n:nat): (a->b) -> cost (option a) n -> cost (option b) n
let map #_ #_ #_ f mx =
  mx `bind` (f >> some)

val ap(#a #b:Type)(#m #n:nat): cost (option (a->b)) m -> cost (option a) n
  -> cost (option b) (n+m)
let ap #_ #_ #_ #_ mf mx =
  mf `bind` (fun f -> map f mx)

val map2(#a #b #c:Type)(#m #n:nat): (a->b->c) -> cost (option a) m -> cost (option b) n
  -> cost (option c) (m+n)
let map2 #_ #_ #_ #_ #_ f mx = ap (map f mx)

val map3(#a #b #c #d:Type)(#n1 #n2 #n3:nat):
  (a->b->c->d) -> cost (option a) n1 -> cost (option b) n2 -> cost (option c) n3
  -> cost (option d) (n1+n2+n3)
let map3 #_ #_ #_ #_ #_ #_ #_ f mx my = ap (map2 f mx my)

val mapBind(#a #b:Type)(#n:nat):
  (a -> option b) -> cost (option a) n -> cost (option b) n
let mapBind #_ #_ #_ f mx = mx `bind` (f >> ret)

val retBind(#a #b:Type)(#n:nat):
  (option a) -> (a -> cost (option b) n) -> cost (option b) n
let retBind #_ #_ #n = ret >> bind

val bindLift(#a #b:Type)(#m #n:nat):
  cost (option a) n -> (a -> cost b m) -> cost (option b) (n+m)
let bindLift #_ #_ #_ #_ mx f = bind mx (f >> lift)

val bindLift2(#a #b #c:Type)(#n1 #n2 #n3:nat):
  cost (option a) n1 -> cost (option b) n2
  -> (a -> b -> cost c n3) -> cost (option c) (n1+n2+n3)
let bindLift2 #_ #_ #_ #_ #_ #_ mx my f = bind2 mx my (fun x y -> lift (f x y))

val bindLift3(#a #b #c #d:Type)(#n1 #n2 #n3 #n4:nat):
  cost (option a) n1 -> cost (option b) n2 -> cost (option c) n3
  -> (a -> b -> c -> cost d n4) -> cost (option d) (n1+n2+n3+n4)
let bindLift3 #_ #_ #_ #_ #_ #_ #_ #_ mx my mz f =
  bind3 mx my mz (fun x y z -> lift (f x y z))

unfold let (~!?) = some
unfold let (+~!) = incSome // infix
unfold let (~+!) = incSome // prefix
unfold let (>>=) = bind
unfold let (=<<) f x = bind x f
unfold let (<$>) = map
unfold let (<$$>) = map2
unfold let (<$$$>) = map3
unfold let (<*>) = ap
unfold let ( *>) x f = ap f x
unfold let ($>) x f = liftM f x
val ($$>) (#a #b #c:Type)(#m #n:nat):
  (cost (option a) m * cost (option b) n) -> (a->b->c) -> cost (option c) (m+n)
let ($$>) #_ #_ #_ #_ #_ (mx,my) f = map2 f mx my
val ($$$>) (#a #b #c #d:Type)(#n1 #n2 #n3:nat):
  (cost (option a) n1 * cost (option b) n2 * cost (option c) n3)
  -> (a->b->c->d) -> cost (option d) (n1+n2+n3)
let ($$$>) #_ #_ #_ #_ #_ #_ #_ (mx,my,mz) f = map3 f mx my mz
