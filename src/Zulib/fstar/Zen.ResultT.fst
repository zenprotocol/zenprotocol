module Zen.ResultT

open Zen.Base

module Cost = Zen.Cost
module Res = Zen.Result
module OptT = Zen.OptionT

type resultT (a:Type) (n:nat) = result a `Cost.t` n
type t : Type -> nat -> Type = resultT

val ok(#a:Type): a -> a `resultT` 0
let ok #_ = OK >> Cost.ret

val ret(#a:Type): a -> a `resultT` 0
let ret #_ = ok

val liftRes(#a:Type): result a -> a `resultT` 0
let liftRes #_ = Cost.ret

val liftCost(#a:Type)(#n:nat): a `Cost.t` n -> a `resultT` n
let liftCost #_ #_ = Cost.map OK

val fail: exn -> 'a `resultT` 0
let fail #_ = Res.fail >> liftRes

val failw: string -> 'a `resultT` 0
let failw #_ = Res.failw >> liftRes

val incFail(#a:Type): n:nat -> exn -> a `resultT` n
let incFail #_ n = fail >> Cost.inc n

val incFailw(#a:Type): n:nat -> string -> a `resultT` n
let incFailw #_ n = failw >> Cost.inc n

val incOK(#a:Type): n:nat -> a -> a `resultT` n
let incOK #_ n = ok >> Cost.inc n

val incRet(#a:Type): n:nat -> a -> a `resultT` n
let incRet = incOK

val autoFailw(#a:Type)(#n:nat): string -> a `resultT` n
let autoFailw #_ #_ = failw >> Cost.autoInc

val autoFail(#a:Type)(#n:nat): exn -> a `resultT` n
let autoFail #_ #_ = fail >> Cost.autoInc

val autoOK(#a:Type)(#n:nat): a -> a `resultT` n
let autoOK #_ #_ = ok >> Cost.autoInc

val autoRet(#a:Type)(#n:nat): a -> a `resultT` n
let autoRet #_ #_ = autoOK

val of_option(#a:Type): string -> option a -> a `resultT` 0
let of_option(#_) msg = function
  | Some v -> ok v
  | None -> failw msg

val of_optionT(#a:Type)(#n:nat): string -> a `OptT.t` n -> a `resultT` n
let of_optionT #_ #_ msg mx = mx `Cost.bind` (of_option msg)

val bind(#a #b:Type)(#m #n:nat):
  a `resultT` m
  -> (a -> b `resultT` n)
  -> b `resultT` (m+n)
let bind #_ #_ #_ #n mx f =
  mx `Cost.bind` (function
  | OK x -> f x
  | EX e -> autoFail e
  | ERR msg -> autoFailw msg)

val (>>=) (#a #b:Type)(#m #n:nat):
  a `resultT` m
  -> (a -> b `resultT` n)
  -> b `resultT` (m+n)
let (>>=) = bind

val (=<<) (#a #b:Type)(#m #n:nat):
  (a -> b `resultT` n)
  -> a `resultT` m
  -> b `resultT` (m+n)
let (=<<) #_ #_ #_ #_ f mx = bind mx f

val bind2(#a #b #c:Type)(#n1 #n2 #n3:nat):
  a `resultT` n1
  -> b `resultT` n2
  -> (a -> b -> c `resultT` n3)
  -> c `resultT` (n1+n2+n3)
let bind2 #_ #_ #_ #_ #_ #_ mx my f =
  mx >>= (fun x ->
  my >>= (fun y ->
  f x y))

val bind3(#a #b #c #d:Type)(#n1 #n2 #n3 #n4:nat):
  a `resultT` n1
  -> b `resultT` n2
  -> c `resultT` n3
  -> (a -> b -> c -> d `resultT` n4)
  -> d `resultT` (n1+n2+n3+n4)
let bind3 #_ #_ #_ #_ #_ #_ #_ #_ mx my mz f =
  mx >>= (fun x ->
  my >>= (fun y ->
  mz >>= (fun z ->
  f x y z)))

val join(#a:Type)(#m #n:nat): (a `resultT` n) `resultT` m -> a `resultT` (m+n)
let join #_ #_ #_ x =
  x >>= (fun z -> z)

val map(#a #b:Type)(#n:nat): (a -> b) -> a `resultT` n -> b `resultT` n
let map #_ #_ #_ f mx =
  mx >>= (f >> ok)

val (<$>) (#a #b:Type)(#n:nat): (a -> b) -> a `resultT` n -> b `resultT` n
let (<$>) = map

val ( $>) (#a #b:Type)(#n:nat): a `resultT` n -> (a -> b) -> b `resultT` n
let ( $>) #_ #_ #_ mx f = f `map` mx

val ap(#a #b:Type)(#m #n:nat):
  (a->b) `resultT` m
  -> a `resultT` n
  -> b `resultT` (m+n)
let ap #_ #_ #_ #_ mf mx =
  mf >>= (fun f -> f `map` mx)

val (<*>) (#a #b:Type)(#m #n:nat):
  (a->b) `resultT` m
  -> a `resultT` n
  -> b `resultT` (m+n)
let (<*>) = ap

val ( *>) (#a #b:Type)(#m #n:nat):
  a `resultT` m
  -> (a->b) `resultT` n
  -> b `resultT` (m+n)
let ( *>) #_ #b #m #n mx mf = ap mf mx <: b `resultT` (n+m)

val (<~>) (#a #b:Type)(#n:nat):
  (a->b) `resultT` n
  -> a
  -> b `resultT` n
let (<~>) #_ #_ #_ mf = ok >> ap mf

val (>=>) (#a #b #c:Type)(#m #n:nat):
  (a -> b `resultT` m) -> (b -> c `resultT` n) -> (a -> c `resultT` (m+n))
let (>=>) #_ #_ #_ #_ #_ f g =
  fun x -> f x >>= g

val (<=<) (#a #b #c:Type)(#m #n:nat):
  (b -> c `resultT` n) -> (a -> b `resultT` m) -> (a -> c `resultT` (m+n))
let (<=<) #_ #_ #_ #_ #_ g f = f >=> g
