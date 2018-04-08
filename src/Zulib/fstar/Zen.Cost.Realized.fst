module Zen.Cost.Realized

open Zen.Base
abstract type cost (a:Type) (n:nat) =
  | C : inj:a -> cost a n
assume Cost_hasEq: forall a n. hasEq a ==>  hasEq (cost a n)

type t (a:Type) (n:nat) = cost a n

//
// Monadic Return
//

abstract val ret(#a:Type): a ->  cost a 0
abstract let ret #_ x = C x

//
// Monadic Bind
//

abstract val bind(#a #b:Type)(#m #n:nat):
  cost a m -> (a -> cost b n) -> cost b (m+n)
abstract let bind #_ #_ #_ #_ (C x) f = C (C?.inj (f x))

//
// Monadic Increment
//

abstract val inc(#a:Type)(#n: nat): cost a n -> k: nat -> cost a (n+k)
abstract let inc #_ #_ (C x) k = C x


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
abstract val force(#a:Type)(#n:nat): cost a n -> GTot a
abstract let force #_ #_ mx = C?.inj mx

val force_ret(#a:Type): x:a -> Lemma (force (ret x) == x)
                               [SMTPat (ret x)]
let force_ret #_ x = ()
val force_inc(#a:Type)(#m:nat):
  n:nat -> mx: cost a m -> Lemma(force (inc mx n) == force mx)
                           [SMTPat (inc mx n)]
let force_inc #_ #_ _ _ = ()
val force_bind(#a #b:Type)(#m #n:nat): mx:cost a m -> f:(a -> cost b n)
  -> Lemma(force (f (force mx)) == force (bind mx f))
     [SMTPat (bind mx f)]
let force_bind #_ #_ #_ #_ _ _ = ()

val refine: a:Type u#t -> p:(a -> prop) -> Type u#t
let refine a p = x:a{p x}

assume val force_prop(#a:Type):
    p:(a -> prop)
    -> Lemma (forall (n:nat). cost (x:a{p x}) n == mx:cost a n{p (force mx)})

val force_bind_inc(#a #b:Type)(#m #n:nat):
    mx:cost a m
    -> f:(a -> cost b n)
    -> Lemma (mx `bind` f == inc (f (force mx)) m)
let force_bind_inc #_ #_ #_ #_ _ _ = ()
