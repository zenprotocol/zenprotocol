(** Cost model **)
module Zen.Cost

open Zen.Base
abstract type cost (a:Type) (n:nat) =
  | C : inj:a -> cost a n
assume Cost_hasEq: forall a n. hasEq a ==>  hasEq (cost a n)

//
// Monadic Return
//

abstract val ret(#a:Type): a ->  cost a 0
abstract let ret #_ x = C x

//
// Monadic Bind
//

abstract val bind(#a #b:Type)(#m #n:nat):
  cost a n -> (a -> cost b m) -> cost b (n+m)
abstract let bind #_ #_ #_ #_ (C x) f = C (C?.inj (f x))

//
// Monadic Increment
//

abstract val inc(#a:Type)(#n: nat): cost a n -> k: nat -> cost a (n+k)
abstract let inc #_ #_ (C x) k = C x

//
// Derived Operations
//

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

val liftM(#a #b:Type)(#n:nat): (a->b) -> cost a n -> cost b n
let liftM #_ #_ #_ f mx =
  mx `bind` (f >> ret)

val ap(#a #b:Type)(#m #n:nat): cost (a->b) n -> cost a m -> cost b (n+m)
let ap #_ #_ #_ #_ mf mx =
  mf `bind` (fun f -> liftM f mx)

val liftM2(#a #b #c:Type)(#m #n:nat): (a->b->c) -> cost a m -> cost b n
  -> cost c (m+n)
let liftM2 #_ #_ #_ #_ #_ f = liftM f >> ap

val liftM3(#a #b #c #d:Type)(#n1 #n2 #n3:nat):
  (a -> b -> c -> d)
  -> cost a n1 -> cost b n2 -> cost c n3
  -> cost d (n1+n2+n3)
let liftM3 #_ #_ #_ #_ #_ #_ #_ f mx my mz =
  f `liftM` mx `ap` my `ap` mz

// The "fish". Left to right Kleisli composition of cost.
val (>=>) (#a #b #c:Type)(#m #n:nat):
  (a -> cost b n) -> (b -> cost c m) -> (a -> cost c (n+m))
let (>=>) #_ #_ #_ #_ #_ f g = fun x -> bind (f x) g

// Right to left Kleisli composition of cost.
val (<=<) (#a #b #c:Type)(#m #n:nat):
  (b -> cost c n) -> (a -> cost b m) -> (a-> cost c (n+m))
let (<=<) #_ #_ #_ #_ #_ f g = g >=> f

val eq(#a:eqtype)(#m #n:nat): cost a m -> cost a n -> cost bool (n+m)
let eq #_ #_ #_ = liftM2 op_Equality

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
unfold let (+!) k x = inc x k
unfold let (+~!) = incRet // infix
unfold let (~+!) = incRet // prefix
unfold let (>>=) = bind
unfold let (=<<) f x = bind x f
unfold let (<$>) = liftM
unfold let (<$$>) = liftM2
unfold let (<$$$>) = liftM3
unfold let (<*>) = ap
unfold let ( *>) x f = ap f x
unfold let (<=>) = eq
unfold let ($>) x f = liftM f x
val ($$>) (#a #b #c:Type)(#m #n:nat):
  (cost a m * cost b n) -> (a->b->c) -> cost c (m+n)
let ($$>) #_ #_ #_ #_ #_ (mx,my) f = liftM2 f mx my
val ($$$>) (#a #b #c #d:Type)(#n1 #n2 #n3:nat):
  (cost a n1 * cost b n2 * cost c n3) -> (a->b->c->d) -> cost d (n1+n2+n3)
let ($$$>) #_ #_ #_ #_ #_ #_ #_ (mx,my,mz) f = liftM3 f mx my mz
val (<~>) (#a #b:Type)(#n:nat): cost (a->b) n -> a -> cost b n
let (<~>) #_ #_ #_ mf = ret >> ap mf

//
// Monad Laws
//

val left_id(#a #b:Type)(#n:nat): x:a -> f:(a -> cost b n)
  -> Lemma (ret x `bind` f == f x )
let left_id #_ #_ #_ _ _ = ()
val right_id(#a:Type)(#n:nat): mx:cost a n
  -> Lemma (mx `bind` ret == mx)
let right_id #_ #_ _ = ()
val assoc(#a #b #c:Type)(#n1 #n2 #n3:nat):
  m:cost a n1 -> f:(a -> cost b n2) -> g:(b -> cost c n3)
  -> Lemma ((m `bind` f) `bind` g == m `bind` (fun x -> f x `bind` g))
let assoc #_ #_ #_ #_ #_ #_ _ _ _ = ()

// These functions are intentionally not implemented. They cannot be used in actual code.
// Nevertheless, they are very useful for theorem proving.

val force(#a:Type)(#n:nat): cost a n -> a
let force #_ #_ mx = C?.inj mx
val forceT(#n:nat): cost bool n -> GTot Type0
let forceT #_ mx = force mx == true
val force_ret(#a:Type): x:a -> Lemma (force (ret x) == x)
                               [SMTPat (ret x)]
let force_ret #_ x = ()
val force_inc(#a:Type)(#m:nat):
  n:nat -> mx: cost a m -> Lemma(force (inc mx n) == force mx)
                           [SMTPat (inc mx n); SMTPat (n +! mx)]
let force_inc #_ #_ _ _ = ()
val force_retInc(#a:Type): n:nat -> x:a -> Lemma (force (incRet n x) == x)
                                           [SMTPat (incRet n x)]
let force_retInc #_ _ _ = ()
val force_bind(#a #b:Type)(#m #n:nat): mx:cost a m -> f:(a -> cost b n)
  -> Lemma(force (f (force mx)) == force (bind mx f))
     [SMTPat (bind mx f); SMTPat (mx >>= f); SMTPat (f =<< mx)]
let force_bind #_ #_ #_ #_ _ _ = ()
val force_liftM(#a #b:Type)(#n:nat): f:(a -> b) -> mx: cost a n
  -> Lemma( f (force mx) == force (liftM f mx))
     [SMTPat (liftM f mx ); SMTPat (f <$> mx); SMTPat (mx $> f)]
let force_liftM #_ #_ #_ _ _ = ()
val force_ap: #a:Type -> #b:Type -> #m:nat -> #n:nat
  -> mf:cost (a -> b) n -> mx: cost a m
  -> Lemma( (force mf) (force mx) == force (ap mf mx))
     [SMTPat (ap mf mx); SMTPat (mf <*> mx)]
let force_ap #_ #_ #_ #_ _ _ = ()
