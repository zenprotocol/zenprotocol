(** Some functions for the Option type *)
module Zen.Option
open Zen.Base

val maybe(#a #b:Type): b -> (a -> b) -> option a -> b
let maybe #_ #_ y f = function | None -> y
                               | Some x -> f x

val fromOption(#a:Type): a -> option a -> a
let fromOption #_ x = function | None -> x
                            | Some y -> y

val bind(#a #b:Type): option a -> (a -> option b) -> option b
let bind #_ #_ mx f = match mx with
  | Some x -> f x
  | None -> None

val map(#a #b:Type): (a -> b) -> option a -> option b
let map #_ #_ f mx = bind mx (f >> Some)

val ( >>? ) (#a #b #c:Type): (a->option b) -> (b->option c) -> a -> option c
let ( >>? ) #_ #_ #_ f g x = (f x) `bind` g

val ap(#a #b:Type): option (a->b) -> option a -> option b
let ap #_ #_ mf mx = mf `bind` (fun f -> f `map` mx)

val join(#a:Type): option (option a) -> option a
let join #_ mmx = mmx `bind` (fun x -> x)

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
