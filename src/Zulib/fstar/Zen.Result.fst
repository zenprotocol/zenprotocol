module Zen.Result
open Zen.Base

type t : Type -> Type = result

val ret(#a:Type): a -> result a
let ret(#_) = OK

val fail: exn -> result 'a
let fail e = EX e

val failw: string -> result 'a
let failw msg = ERR msg

val of_option(#a:Type): string -> option a -> result a
let of_option(#_) msg = function
  | Some v -> ret v
  | None -> failw msg

val bind(#a #b:Type): result a -> (a -> result b) -> result b
let bind #_ #_ mx f =
  match mx with
  | OK x -> f x
  | EX e -> fail e
  | ERR msg -> failw msg

val (>>=) (#a #b:Type): result a -> (a -> result b) -> result b
let (>>=) = bind

val (=<<) (#a #b:Type): (a -> result b) -> result a -> result b
let (=<<) #_ #_ f mx = mx >>= f

val bind2(#a #b #c:Type):
  result a -> result b -> (a -> b -> result c) -> result c
let bind2 #_ #_ #_ mx my f =
  mx >>= (fun x ->
  my >>= (fun y ->
  f x y))

val bind3(#a #b #c #d:Type):
  result a -> result b -> result c -> (a -> b -> c -> result d) -> result d
let bind3 #_ #_ #_ #_ mx my mz f =
  mx >>= (fun x ->
  my >>= (fun y ->
  mz >>= (fun z ->
  f x y z)))

val join(#a:Type): result (result a) -> result a
let join(#_) mx = mx >>= (fun z -> z)

val map(#a #b:Type): (a -> b) -> result a -> result b
let map #_ #_ f mx =
  mx >>= (f >> ret)

val (<$>) (#a #b:Type): (a->b) -> result a -> result b
let (<$>) = map

val ($>) (#a #b:Type): result a -> (a->b) -> result b
let ($>) #_ #_ x f = map f x

val ap(#a #b:Type): result (a -> b) -> result a -> result b
let ap #_ #_ mf mx =
  mf >>= (fun f -> map f mx)

val (<*>) (#a #b:Type): result (a->b) -> result a -> result b
let (<*>) = ap

val ( *>) (#a #b:Type): result a -> result (a->b) -> result b
let ( *>) #_ #_ mx mf = ap mf mx

val (<~>) (#a #b:Type): result (a->b) -> a -> result b
let (<~>) #_ #_ mf = ret >> ap mf

val (>=>) (#a #b #c:Type):
  (a -> result b) -> (b -> result c) -> (a -> result c)
let (>=>) #_ #_ #_ f g =
  fun x -> f x >>= g

val (<=<) (#a #b #c:Type):
  (b -> result c) -> (a -> result b) -> (a-> result c)
let (<=<) #_ #_ #_ g f = f >=> g
