module Zen.Error
open Zen.Base

val ret(#a:Type): a -> result a
let ret(#_) = OK

val fail: exn -> result 'a
let fail e = EX e

val failw: string -> result 'a
let failw msg = ERR msg

val bind(#a #b:Type): result a -> (a -> result b) -> result b
let bind #_ #_ mx f =
  match mx with
  | OK x -> f x
  | EX e -> fail e
  | ERR msg -> failw msg

val map(#a #b:Type): (a -> b) -> result a -> result b
let map #_ #_ f mx =
  bind mx (ret << f)

val ap(#a #b:Type): result (a -> b) -> result a -> result b
let ap #_ #_ mf mx =
  bind mf (fun f -> map f mx)

val join(#a:Type): result (result a) -> result a
let join(#_) = function
  | OK mx -> mx
  | EX e -> fail e
  | ERR msg -> failw msg

val bind2(#a #b #c:Type):
  result a -> result b -> (a -> b -> result c) -> result c
let bind2 #_ #_ #_ mx my f =
  mx `bind` (fun x ->
  my `bind` (fun y -> f x y))

val bind3(#a #b #c #d:Type):
  result a -> result b -> result c -> (a -> b -> c -> result d) -> result d
let bind3 #_ #_ #_ #_ mx my mz f =
  mx `bind` (fun x ->
  my `bind` (fun y ->
  mz `bind` (fun z -> f x y z)))

val map2(#a #b #c:Type):
  (a -> b -> c) -> result a -> result b -> result c
let map2 #_ #_ #_ f = map f >> ap

val map3(#a #b #c #d:Type):
  (a -> b -> c -> d) -> result a -> result b -> result c -> result d
let map3 #_ #_ #_ #_ f mx my mz =
  f `map` mx `ap` my `ap` mz

val (>=>) (#a #b #c:Type):
  (a -> result b) -> (b -> result c) -> (a -> result c)
let (>=>) #_ #_ #_ f g =
  fun x -> bind (f x) g

val (<=<) (#a #b #c:Type):
  (b -> result c) -> (a -> result b) -> (a-> result c)
let (<=<) #_ #_ #_ f g = g >=> f

val of_option(#a:Type): string -> option a -> result a
let of_option(#_) msg = function
  | Some v -> ret v
  | None -> failw msg
