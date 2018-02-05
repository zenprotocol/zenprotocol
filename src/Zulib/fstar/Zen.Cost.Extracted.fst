module Zen.Cost.Extracted

open Zen.Base
open Zen.Cost.Realized

val bind2(#a #b #c:Type)(#n1 #n2 #n3:nat):
  cost a n1 -> cost b n2 -> (a -> b -> cost c n3) -> cost c (n1+n2+n3)
let bind2 #_ #_ #_ #_ #_ #_ mx my f = mx `bind` (fun x ->
                                      my `bind` (fun y ->
                                      f x y))

val bind3(#a #b #c #d:Type)(#n1 #n2 #n3 #n4:nat):
  cost a n1 -> cost b n2 -> cost c n3 -> (a -> b -> c -> cost d n4)
  -> cost d (n1+n2+n3+n4)
let bind3 #_ #_ #_ #_ #_ #_ #_ #_ mx my mz f = mx `bind` (fun x ->
                                               my `bind` (fun y ->
                                               mz `bind` (fun z ->
                                               f x y z)))

val join(#a:Type)(#m #n:nat): cost (cost a n) m -> cost a (n+m)
let join #_ #_ #_ x =
  x `bind` (fun z -> z)

val map(#a #b:Type)(#n:nat): (a->b) -> cost a n -> cost b n
let map #_ #_ #_ f mx =
  mx `bind` (f >> ret)

val ap(#a #b:Type)(#m #n:nat): cost (a->b) n -> cost a m -> cost b (n+m)
let ap #_ #_ #_ #_ mf mx =
  mf `bind` (fun f -> map f mx)

val map2(#a #b #c:Type)(#m #n:nat): (a->b->c) -> cost a m -> cost b n
  -> cost c (m+n)
let map2 #_ #_ #_ #_ #_ f = map f >> ap

val map3(#a #b #c #d:Type)(#n1 #n2 #n3:nat):
  (a -> b -> c -> d)
  -> cost a n1 -> cost b n2 -> cost c n3
  -> cost d (n1+n2+n3)
let map3 #_ #_ #_ #_ #_ #_ #_ f mx my mz =
  f `map` mx `ap` my `ap` mz

// The "fish". Left to right Kleisli composition of cost.
val (>=>) (#a #b #c:Type)(#m #n:nat):
  (a -> cost b n) -> (b -> cost c m) -> (a -> cost c (n+m))
let (>=>) #_ #_ #_ #_ #_ f g = fun x -> bind (f x) g

// Right to left Kleisli composition of cost.
val (<=<) (#a #b #c:Type)(#m #n:nat):
  (b -> cost c n) -> (a -> cost b m) -> (a-> cost c (n+m))
let (<=<) #_ #_ #_ #_ #_ f g = g >=> f

val incRet(#a:Type): n:nat -> a -> cost a n
let incRet(#_) n x = inc (ret x) n

(** autoInc adds cost to even out branches.*)
val autoInc(#a:Type)(#m:nat)(#n:nat{m<=n}): cost a m -> cost a n
let autoInc #_ #m #n mx = inc mx (n-m)

val autoRet(#a:Type)(#m:nat): a -> cost a m
let autoRet #_ #_ = ret >> autoInc

val retf(#a #b:Type): (a -> b) -> (a -> cost b 0)
let retf #_ #_ f = f >> ret

//
// Operators
//
unfold let (~!) = ret

unfold val (+!): #a:Type -> #n:nat -> k:nat -> cost a n -> cost a (n+k)
unfold let (+!) #_ #_ k x = inc x k

unfold let (+~!) = incRet // infix
unfold let (~+!) = incRet // prefix

unfold val (>>=) (#a #b:Type)(#m #n:nat):
  cost a n -> (a -> cost b m) -> cost b (n+m)
unfold let (>>=) #_ #_ #_ #_ mx f = bind mx f

unfold val (=<<) (#a #b:Type)(#m #n:nat):
  (a -> cost b m) -> cost a n -> cost b (n+m)
unfold let (=<<) #_ #_ #_ #_ f mx = bind mx f

unfold val (<$>) (#a #b:Type)(#n:nat): (a->b) -> cost a n -> cost b n
unfold let (<$>) #_ #_ #_ f mx = map f mx

unfold val ($>) (#a #b:Type)(#n:nat): cost a n -> (a->b) -> cost b n
unfold let ($>) #_ #_ #_ x f = map f x

unfold val (<$$>) (#a #b #c:Type)(#m #n:nat):
  (a->b->c) -> (cost a m ** cost b n) -> cost c (m+n)
unfold let (<$$>) #_ #_ #_ #_ #_ f (mx, my) = map2 f mx my

unfold val ($$>) (#a #b #c:Type)(#m #n:nat):
  (cost a m ** cost b n) -> (a->b->c) -> cost c (m+n)
unfold let ($$>) #_ #_ #_ #_ #_ (mx, my) f = f <$$> (mx, my)

unfold val (<$$$>) (#a #b #c #d:Type)(#n1 #n2 #n3:nat):
  (a->b->c->d) -> (cost a n1 ** cost b n2 ** cost c n3) -> cost d (n1+n2+n3)
unfold let (<$$$>) #_ #_ #_ #_ #_ #_ #_ f (mx, my, mz) = map3 f mx my mz

unfold val ($$$>) (#a #b #c #d:Type)(#n1 #n2 #n3:nat):
  (cost a n1 ** cost b n2 ** cost c n3) -> (a->b->c->d) -> cost d (n1+n2+n3)
unfold let ($$$>) #_ #_ #_ #_ #_ #_ #_ (mx, my, mz) f = f <$$$> (mx, my, mz)

unfold val (<*>) (#a #b:Type)(#m #n:nat): cost (a->b) n -> cost a m -> cost b (n+m)
unfold let (<*>) #_ #_ #_ #_ mf mx = ap mf mx

unfold val ( *>) (#a #b:Type)(#m #n:nat): cost a m -> cost (a->b) n -> cost b (n+m)
unfold let ( *>) #_ #_ #_ #_ mx mf = ap mf mx

val (<~>) (#a #b:Type)(#n:nat): cost (a->b) n -> a -> cost b n
let (<~>) #_ #_ #_ mf = ret >> ap mf

// These functions are intentionally not implemented. They cannot be used in actual code.
// Nevertheless, they are very useful for theorem proving.
val force_map(#a #b:Type)(#n:nat): f:(a -> b) -> mx: cost a n
  -> Lemma( f (force mx) == force (map f mx))
     [SMTPat (map f mx ); SMTPat (f <$> mx); SMTPat (mx $> f)]
let force_map #_ #_ #_ _ _ = ()
val force_ap: #a:Type -> #b:Type -> #m:nat -> #n:nat
  -> mf:cost (a -> b) n -> mx: cost a m
  -> Lemma( (force mf) (force mx) == force (ap mf mx))
     [SMTPat (ap mf mx); SMTPat (mf <*> mx)]
let force_ap #_ #_ #_ #_ _ _ = ()
