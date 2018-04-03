(** Some functions for the Option type *)
module Zen.Option
open Zen.Base

type t : Type -> Type = option

val maybe(#a #b:Type): b -> (a -> b) -> option a -> b
let maybe #_ #_ y f = function | None -> y
                               | Some x -> f x

val fromOption(#a:Type): a -> option a -> a
let fromOption #_ x = function | None -> x
                               | Some y -> y

val ret(#a:Type): a -> option a
let ret #_ = Some

val bind(#a #b:Type): option a -> (a -> option b) -> option b
let bind #_ #_ mx f = match mx with
  | Some x -> f x
  | None -> None

val (>>=) (#a #b:Type): option a -> (a -> option b) -> option b
let (>>=) = bind

val (=<<) (#a #b:Type): (a -> option b) -> option a -> option b
let (=<<) #_ #_ f mx = mx >>= f

val bind2(#a #b #c:Type):
  option a -> option b -> (a -> b -> option c) -> option c
let bind2 #_ #_ #_ mx my f =
  mx >>= (fun x ->
  my >>= (fun y ->
  f x y))

val bind3(#a #b #c #d:Type):
  option a -> option b -> option c -> (a -> b -> c -> option d) -> option d
let bind3 #_ #_ #_ #_ mx my mz f =
  mx >>= (fun x ->
  my >>= (fun y ->
  mz >>= (fun z ->
  f x y z)))

val join(#a:Type): option (option a) -> option a
let join(#_) mx = mx >>= (fun z -> z)

val map(#a #b:Type): (a -> b) -> option a -> option b
let map #_ #_ f mx =
  mx >>= (f >> ret)

val (<$>) (#a #b:Type): (a->b) -> option a -> option b
let (<$>) = map

val ($>) (#a #b:Type): option a -> (a->b) -> option b
let ($>) #_ #_ x f = map f x

val ap(#a #b:Type): option (a -> b) -> option a -> option b
let ap #_ #_ mf mx =
  mf >>= (fun f -> map f mx)

val (<*>) (#a #b:Type): option (a->b) -> option a -> option b
let (<*>) = ap

val ( *>) (#a #b:Type): option a -> option (a->b) -> option b
let ( *>) #_ #_ mx mf = ap mf mx

val (<~>) (#a #b:Type): option (a->b) -> a -> option b
let (<~>) #_ #_ mf = ret >> ap mf

val (>=>) (#a #b #c:Type):
  (a -> option b) -> (b -> option c) -> (a -> option c)
let (>=>) #_ #_ #_ f g =
  fun x -> f x >>= g

val (<=<) (#a #b #c:Type):
  (b -> option c) -> (a -> option b) -> (a-> option c)
let (<=<) #_ #_ #_ g f = f >=> g

(*)
                      Helicowpters

           _- --/   - `\
      _________/_________           \    |   /
      "       /|                      \  | /
(__)   `     /_| _ -              _____ ___ _____
|@@|           |            _         /(o o) \
 \/`--------.__/           (_)------/---\ /    \
  |         /                    |     ||
  //`--\/-||                     ||----||
  ||      ||                     oo    oo
