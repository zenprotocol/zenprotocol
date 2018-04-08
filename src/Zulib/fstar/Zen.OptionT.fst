(** This module defines monad transformers for option and cost. *)
module Zen.OptionT

open Zen.Base
module Cost = Zen.Cost
module Opt = Zen.Option

type optionT (a:Type) (n:nat) = option a `Cost.t` n
type t : Type -> nat -> Type = optionT

val some(#a:Type): a -> a `optionT` 0
let some #_ = Some >> Cost.ret

val ret(#a:Type): a -> a `optionT` 0
let ret #_ = some

val none(#a:Type): a `optionT` 0
let none #_ = Cost.ret None

val liftOpt(#a:Type): option a -> a `optionT` 0
let liftOpt #_ = Cost.ret

val liftCost(#a:Type)(#n:nat): a `Cost.t` n -> a `optionT` n
let liftCost #_ #_ = Cost.map Some

val incNone(#a:Type): n:nat -> a `optionT` n
let incNone #_ n = none |> Cost.inc n

val incSome(#a:Type): n:nat -> a -> a `optionT` n
let incSome(#_) n = some >> Cost.inc n

val autoNone(#a:Type)(#n:nat): a `optionT` n
let autoNone #_ #_ = Cost.autoInc none

val autoSome(#a:Type)(#n:nat): a -> a `optionT` n
let autoSome #_ #_ = some >> Cost.autoInc

val autoRet(#a:Type)(#n:nat): a -> a `optionT` n
let autoRet #_ #_ = autoSome

val bind(#a #b:Type)(#m #n:nat):
  a `optionT` m
  -> (a -> b `optionT` n)
  -> b `optionT` (m+n)
let bind #_ #_ #_ #n mx f =
  mx `Cost.bind` (function
  | Some x -> f x
  | None -> autoNone)

val (>>=) (#a #b:Type)(#m #n:nat):
  a `optionT` m
  -> (a -> b `optionT` n)
  -> b `optionT` (m+n)
let (>>=) = bind

val (=<<) (#a #b:Type)(#m #n:nat):
  (a -> b `optionT` n)
  -> a `optionT` m
  -> b `optionT` (m+n)
let (=<<) #_ #_ #_ #_ f mx = bind mx f

val bind2(#a #b #c:Type)(#n1 #n2 #n3:nat):
  a `optionT` n1
  -> b `optionT` n2
  -> (a -> b -> c `optionT` n3)
  -> c `optionT` (n1+n2+n3)
let bind2 #_ #_ #_ #_ #_ #_ mx my f =
  mx >>= (fun x ->
  my >>= (fun y ->
  f x y))

val bind3(#a #b #c #d:Type)(#n1 #n2 #n3 #n4:nat):
  a `optionT` n1
  -> b `optionT` n2
  -> c `optionT` n3
  -> (a -> b -> c -> d `optionT` n4)
  -> d `optionT` (n1+n2+n3+n4)
let bind3 #_ #_ #_ #_ #_ #_ #_ #_ mx my mz f =
  mx >>= (fun x ->
  my >>= (fun y ->
  mz >>= (fun z ->
  f x y z)))

val join(#a:Type)(#m #n:nat): (a `optionT` n) `optionT` m -> a `optionT` (m+n)
let join #_ #_ #_ x =
  x >>= (fun z -> z)

val map(#a #b:Type)(#n:nat): (a -> b) -> a `optionT` n -> b `optionT` n
let map #_ #_ #_ f mx =
  mx >>= (f >> some)

val (<$>) (#a #b:Type)(#n:nat): (a -> b) -> a `optionT` n -> b `optionT` n
let (<$>) = map

val ( $>) (#a #b:Type)(#n:nat): a `optionT` n -> (a -> b) -> b `optionT` n
let ( $>) #_ #_ #_ mx f = f `map` mx

val ap(#a #b:Type)(#m #n:nat):
  (a->b) `optionT` m
  -> a `optionT` n
  -> b `optionT` (m+n)
let ap #_ #_ #_ #_ mf mx =
  mf >>= (fun f -> f `map` mx)

val (<*>) (#a #b:Type)(#m #n:nat):
  (a->b) `optionT` m
  -> a `optionT` n
  -> b `optionT` (m+n)
let (<*>) = ap

val ( *>) (#a #b:Type)(#m #n:nat):
  a `optionT` m
  -> (a->b) `optionT` n
  -> b `optionT` (m+n)
let ( *>) #_ #b #m #n mx mf = ap mf mx <: b `optionT` (n+m)

val (<~>) (#a #b:Type)(#n:nat):
  (a->b) `optionT` n
  -> a
  -> b `optionT` n
let (<~>) #_ #_ #_ mf = some >> ap mf

val (>=>) (#a #b #c:Type)(#m #n:nat):
  (a -> b `optionT` m) -> (b -> c `optionT` n) -> (a -> c `optionT` (m+n))
let (>=>) #_ #_ #_ #_ #_ f g =
  fun x -> f x >>= g

val (<=<) (#a #b #c:Type)(#m #n:nat):
  (b -> c `optionT` n) -> (a -> b `optionT` m) -> (a -> c `optionT` (m+n))
let (<=<) #_ #_ #_ #_ #_ g f = f >=> g
